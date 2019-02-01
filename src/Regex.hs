{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
module Regex
  ( NERegex(..)
  , Regex
  , prettyPrint
  , compile
  , prettyPrintInst
  ) where

import qualified Data.IntSet as IntSet
import Data.String
import qualified Text.PrettyPrint.ANSI.Leijen as Pretty

data NERegex
  = Juxta NERegex NERegex
  | Lit Char
  | Alt NERegex NERegex
  | Ques NERegex
  | Star NERegex
  | Plus NERegex
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
    prettyPrint' _ (Lit c)
      | c `elem` "[]()\\*+?|" = '\\' : [c]
      | otherwise = [c]
    prettyPrint' p (Juxta r1 r2) =
      parenthesizeIf (p < 3) (prettyPrint' 3 r1 ++ prettyPrint' 3 r2)
    prettyPrint' p (Alt r1 r2) =
      parenthesizeIf (p < 4) (prettyPrint' 4 r1 ++ "|" ++ prettyPrint' 4 r2)
    prettyPrint' p (Ques r) = parenthesizeIf (p < 2) (prettyPrint' 2 r ++ "?")
    prettyPrint' p (Star r) = parenthesizeIf (p < 2) (prettyPrint' 2 r ++ "*")
    prettyPrint' p (Plus r) = parenthesizeIf (p < 2) (prettyPrint' 2 r ++ "+")

data Inst
  = Match Char
    -- ^ Match a certain character at current position.
  | Jmp Int
    -- ^ Transfer execution of the current thread. The @Int@ is the relative
    -- offset from the next instruction.
  | Fork Int
    -- ^ Fork execution. Schedule a new thread for execution. The @Int@ is the
    -- relative offset from the next instruction.
  deriving Show

compile :: NERegex -> [Inst]
compile (Juxta (compile -> a) (compile -> b)) = a ++ b
compile (Lit c) = [Match c]
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
            Match c -> op "match" Pretty.<+> Pretty.text (show c)
        op = Pretty.fill 8 . Pretty.text
        formatLoc absLoc = Pretty.char 'L' <> Pretty.int absLoc
