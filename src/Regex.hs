{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
module Regex
  ( NERegex(..)
  , Regex
  , prettyPrint
  , compile
  , prettyPrintInst
  , BeginOpts(..)
  , EndOpts(..)
  , evalInsts
  , match
  , search
  , fullMatch
  , generateCCode
  , exampleHtml5EmailRegex
  ) where

import Data.Char
import qualified Data.IntSet as IntSet
import Data.List
import qualified Data.List.NonEmpty as NE
import qualified Data.Sequence as Seq
import Data.String
import qualified Text.PrettyPrint.ANSI.Leijen as Pretty

newtype Chars =
  Chars (NE.NonEmpty Char)
  deriving (Show)

instance IsString Chars where
  fromString [] = error "character group may not be empty"
  fromString ne = Chars (NE.fromList (parse ne))
    where
      parse (a:'-':b:ss) = enumFromTo a b ++ parse ss
      parse (a:ss) = a : parse ss
      parse [] = []

data NERegex
  = Juxta NERegex NERegex
  | Lit Char
  | CharGroup Chars
  | NotCharGroup Chars
  | Alt NERegex NERegex
  | Ques NERegex
  | Star NERegex
  | Plus NERegex
  | Dot
  deriving Show

type Regex = Maybe NERegex

-- | Literals
instance IsString Regex where
  fromString [] = Nothing
  fromString ne = Just (foldr1 Juxta (map Lit ne))

instance IsString NERegex where
  fromString [] = error "empty literal for NERegex"
  fromString ne = foldr1 Juxta (map Lit ne)

prettyPrint :: Regex -> String
prettyPrint Nothing = []
prettyPrint (Just ne) = prettyPrint' 10 ne
  where
    parenthesizeIf :: Bool -> String -> String
    parenthesizeIf b s
      | b = '(' : s ++ ")"
      | otherwise = s
    -- pretty print, taking a precedence level to parenthesize automatically
    prettyPrint' :: Int -> NERegex -> String
    prettyPrint' _ Dot = "."
    prettyPrint' _ (Lit c)
      | c `elem` needEscape = '\\' : [c]
      | otherwise = [c]
      where needEscape :: String
            needEscape = "[]()\\*+?|"
    prettyPrint' _ (CharGroup (Chars cs)) = '[' : concatMap escapeCharForCharClass (NE.toList cs) ++ "]"
    prettyPrint' _ (NotCharGroup (Chars cs)) = "[^" ++ concatMap escapeCharForCharClass (NE.toList cs) ++ "]"
    prettyPrint' p (Juxta r1 r2) =
      parenthesizeIf (p < 3) (prettyPrint' 3 r1 ++ prettyPrint' 3 r2)
    prettyPrint' p (Alt r1 r2) =
      parenthesizeIf (p < 4) (prettyPrint' 4 r1 ++ "|" ++ prettyPrint' 4 r2)
    prettyPrint' p (Ques r) = parenthesizeIf (p < 2) (prettyPrint' 2 r ++ "?")
    prettyPrint' p (Star r) = parenthesizeIf (p < 2) (prettyPrint' 2 r ++ "*")
    prettyPrint' p (Plus r) = parenthesizeIf (p < 2) (prettyPrint' 2 r ++ "+")
    escapeCharForCharClass c
      | c `elem` needEscape = '\\' : [c]
      | otherwise = [c]
      where needEscape :: String
            needEscape = "^-]\\"

data Inst
  = AssertIn Chars
    -- ^ Match a certain group of characters at current position. If it does not
    -- match, the current thread dies; otherwise, the current thread proceeds.
  | AssertNotIn Chars
    -- ^ Inversely match a certain group of characters at current position. If
    -- it does match, the current thread dies; otherwise, the current thread
    -- proceeds.
  | Wait
    -- ^ Block the current thread. Wake up when the next character arrives.
  | Jmp Int
    -- ^ Transfer execution of the current thread. The @Int@ is the relative
    -- offset from the next instruction.
  | Fork Int
    -- ^ Fork execution. Schedule a new thread for execution. The @Int@ is the
    -- relative offset from the next instruction.
  deriving Show

compile :: NERegex -> [Inst]
compile (Juxta (compile -> a) (compile -> b)) = a ++ b
compile (Lit c) = [AssertIn (Chars (c NE.:| [])), Wait]
compile (CharGroup cs) = [AssertIn cs, Wait]
compile (NotCharGroup cs) = [AssertNotIn cs, Wait]
compile Dot = [Wait]
compile (Alt (compile -> a) (compile -> b)) = concat [[Fork (1 + length a)], a, [Jmp (length b)], b]
compile (Ques (compile -> a)) = Fork (length a) : a
compile (Star (compile -> a)) = concat [[Fork (1 + length a)], a, [Jmp (-2 - length a)]]
compile (Plus (compile -> a)) = a ++ [Fork (-1 - length a)]

prettyPrintInst :: [Inst] -> String
prettyPrintInst insts = Pretty.displayS (Pretty.renderPretty 0.8 100 doc) "\n"
  where
    jumpOrForkTargets =
      foldMap
        (\(i, inst) ->
           case inst of
             Jmp j -> IntSet.singleton (i + j + 1)
             Fork j -> IntSet.singleton (i + j + 1)
             _ -> mempty)
        (zip [0 ..] insts)
    doc = Pretty.vsep (zipWith pprInst [0 ..] insts)
    pprInst i inst
      | i `IntSet.member` jumpOrForkTargets = Pretty.fill 10 (formatLoc i <> Pretty.colon) Pretty.<+> instDoc
      | otherwise = Pretty.fill 10 Pretty.space Pretty.<+> instDoc
      where
        instDoc =
          case inst of
            Jmp j -> op "jmp" Pretty.<+> formatLoc (i + j + 1)
            Fork j -> op "fork" Pretty.<+> formatLoc (i + j + 1)
            AssertIn (Chars c) -> op "match" Pretty.<+> Pretty.text (show (NE.toList c))
            AssertNotIn (Chars c) -> op "nomatch" Pretty.<+> Pretty.text (show (NE.toList c))
            Wait -> op "wait"
        op = Pretty.fill 8 . Pretty.text
        formatLoc absLoc = Pretty.char 'L' <> Pretty.int absLoc

data BeginOpts = StartMatchAtBeginning | StartMatchAnywhere

data EndOpts = EndMatchAnywhere | EndMatchAtEnd

-- | Evaluate instructions with a string.
evalInsts :: BeginOpts -> EndOpts -> [Inst] -> String -> Bool
evalInsts begin end (Seq.fromList -> insts) = go mempty (IntSet.singleton 0) mempty
  where
    go :: IntSet.IntSet -> IntSet.IntSet -> IntSet.IntSet -> String -> Bool
    go !executed !runnable !blocked str = case IntSet.minView (runnable IntSet.\\ executed) of
      Nothing -> case str of
        [] -> False
        (_:ss) ->
          let runnable' = case begin of StartMatchAtBeginning -> blocked; StartMatchAnywhere -> IntSet.insert 0 blocked
          in not (IntSet.null runnable') && go mempty runnable' mempty ss
      Just (i, runnable') ->
        let go' = go (IntSet.insert i executed) in
        case Seq.lookup i insts of
        Nothing -> case end of EndMatchAnywhere -> True; EndMatchAtEnd -> null str || go' runnable' blocked str
        Just (AssertIn (Chars cs)) -> case str of
          [] -> go' runnable' blocked str
          (s:_) -> go' (if s `elem` cs then IntSet.insert (i+1) runnable' else runnable') blocked str
        Just (AssertNotIn (Chars cs)) -> case str of
          [] -> go' runnable' blocked str
          (s:_) -> go' (if s `notElem` cs then IntSet.insert (i+1) runnable' else runnable') blocked str
        Just Wait -> go' runnable' (IntSet.insert (i+1) blocked) str
        Just (Jmp j) -> go' (IntSet.insert (i + j + 1) runnable') blocked str
        Just (Fork j) -> go' (runnable' <> IntSet.fromList [i + j + 1, i + 1]) blocked str

match :: NERegex -> String -> Bool
match = evalInsts StartMatchAtBeginning EndMatchAnywhere . compile

search :: NERegex -> String -> Bool
search = evalInsts StartMatchAnywhere EndMatchAnywhere . compile

fullMatch :: NERegex -> String -> Bool
fullMatch = evalInsts StartMatchAnywhere EndMatchAtEnd . compile

-- | Given a key and an 'IntSet.IntSet', find out the index at which this key
-- occurs.
intsetLookupIndex :: Int -> IntSet.IntSet ->Int
intsetLookupIndex i is =
  case IntSet.splitMember i is of
    (_, False, _) -> error "not found"
    (pre, True, _) -> IntSet.size pre

generateCCode :: BeginOpts -> EndOpts -> [Inst] -> String
generateCCode begin end insts
  | length instBlock > 63 = error "Instructions too long; not yet implemented"
  | otherwise = unlines prologue ++ middle ++ unlines epilogue
  where
    instBlock :: [NE.NonEmpty Inst]
    instBlock =
      (fmap . fmap)
        fst
        (foldr
           (\ia groups ->
              case groups of
                [] -> [ia NE.:| []]
                g@((_, first) NE.:| _):gs ->
                  if first
                    then (ia NE.:| []) : groups
                    else (ia NE.<| g) : gs)
           []
           annotated)
      where
        annotated = map (\(i, inst) -> (inst, i `IntSet.member` blockBegin)) (zip [0 ..] insts)
    blockBegin =
      IntSet.insert 0 $
      foldMap
        (\(i, inst) ->
           case inst of
             Jmp j -> IntSet.singleton (i + j + 1)
             Fork j -> IntSet.singleton (i + j + 1)
             Wait -> IntSet.singleton (i + 1)
             _ -> mempty)
        (zip [0 ..] insts)
    prologue =
      [ "#ifndef GENERATED_REGEX_H"
      , "#define GENERATED_REGEX_H"
      , ""
      , "#include \"utf8dec.h\""
      , "#include <stdbool.h>"
      , ""
      , ""
      , "/* Global variable for NFA state. */"
      , "static unsigned long long runnable, blocked;"
      , ""
      , "/* Result of running the NFA for one step (one codepoint). */"
      , "enum OneStepResult { DEFINITE_FALSE, DEFINITE_TRUE, NEED_MORE_CHAR };"
      , ""
      , "/* Run the NFA for one code point."
      , "   Raw instructions:"
      , prettyPrintInst insts
      , " */"
      , "static inline enum OneStepResult eval_one_codepoint(unsigned codepoint) {"
      , "    unsigned long long executed = 0;"
      , "    while (1) {"
      , "        runnable &= ~executed;"
      , "        if (!runnable) {"
      , "            if (codepoint) {"
      , case begin of
          StartMatchAtBeginning -> "                runnable = blocked;"
          StartMatchAnywhere -> "                runnable = blocked | 1;"
      , "                blocked = 0;"
      , "                return runnable ? NEED_MORE_CHAR : DEFINITE_FALSE;"
      , "            } else {"
      , "                return DEFINITE_FALSE;"
      , "            }"
      , "        } else {"
      , "            int i = __builtin_ctzll(runnable);"
      , "            runnable &= ~(1ull << i);"
      , "            switch (i) {"
      ]
    middle =
      concat $
      snd $
      mapAccumL
        (\s (bi, NE.toList -> block) ->
           (s + length block, ) $
           unlines $
           ["            case " ++ show bi ++ ":", "                executed |= (1ull << " ++ show bi ++ ");"] ++
           concatMap
             (\(i, inst) ->
                case inst of
                  Fork j ->
                    [ "                runnable |= (1ull << " ++
                      show (intsetLookupIndex (s + i + j + 1) blockBegin) ++ "); // fork"
                    ]
                  Jmp j ->
                    [ "                runnable |= (1ull << " ++
                      show (intsetLookupIndex (s + i + j + 1) blockBegin) ++ "); // jump"
                    , "                continue;"
                    ]
                  AssertNotIn (Chars cs) ->
                    [ "                if (" ++
                      generateCharGroup cs ++ ") { continue; } // assert equal to " ++ show (NE.toList cs)
                    ]
                  AssertIn (Chars cs) ->
                    [ "                if (!(" ++
                      generateCharGroup cs ++ ")) { continue; } // assert equal to " ++ show (NE.toList cs)
                    ]
                  Wait ->
                    [ "                blocked |= (1ull << " ++
                      show (intsetLookupIndex (s + i + 1) blockBegin) ++ ");  // wait for next"
                    , "                continue;"
                    ])
             (zip [0 ..] block))
        0
        (zip [(0 :: Int) ..] instBlock)
    epilogue =
      [ "            default:"
      , "                executed |= (1ull << " ++ show (length instBlock) ++ ");"
      , case end of
          EndMatchAnywhere -> "                return DEFINITE_TRUE;"
          EndMatchAtEnd -> "                if (!codepoint) { return DEFINITE_TRUE; } else { continue; }"
      , "            }"
      , "        }"
      , "    }"
      , "}"
      , ""
      , "/* Evaluate regular expression with entire string. */"
      , "static inline bool eval(unsigned char const* str) {"
      , "    runnable = 1;"
      , "    blocked = 0;"
      , "    while (1) {"
      , "        unsigned codepoint = utf8_decode(&str);"
      , "        switch (eval_one_codepoint(codepoint)) {"
      , "        case DEFINITE_FALSE: return false;"
      , "        case DEFINITE_TRUE: return true;"
      , "        case NEED_MORE_CHAR:"
      , "          // do-nothing"
      , "          ;"
      , "        }"
      , "    }"
      , "}"
      , ""
      , "#endif"
      ]
    generateCharGroup cs =
      foldr1
        (\a b -> a ++ " || " ++ b)
        (fmap
           (\(c, d) ->
              if c == d
                then "codepoint == " ++ show c
                else "(codepoint >= " ++ show c ++ " && codepoint <= " ++ show d ++ ")")
           (toGroup sorted))
      where
        sorted = IntSet.toList (IntSet.fromList (map ord (NE.toList cs)))
        toGroup :: [Int] -> [(Int, Int)]
        toGroup =
          foldr
            (\c gs ->
               case gs of
                 ((d, e):gr)
                   | c + 1 == d -> (c, e) : gr
                 _ -> (c, c) : gs)
            []


exampleHtml5EmailRegex :: NERegex
exampleHtml5EmailRegex =
  Plus (CharGroup "a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-") `Juxta` "@" `Juxta` CharGroup "a-zA-Z0-9" `Juxta`
  Ques (Star (CharGroup "a-zA-Z0-9-") `Juxta` CharGroup "a-zA-Z0-9") `Juxta`
  Star ("." `Juxta` CharGroup "a-zA-Z0-9" `Juxta` Ques (Star (CharGroup "a-zA-Z0-9-") `Juxta` CharGroup "a-zA-Z0-9"))
