{-# LANGUAGE FlexibleInstances #-}
module Regex
  ( NERegex(..)
  , Regex
  , prettyPrint
  ) where

import Data.String

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
  fromString [] = error "empty"
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
