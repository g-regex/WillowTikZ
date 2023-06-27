{-
WillowTikZ.
Copyright (C) 2022  Gregor Feierabend

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
-}

{-|
Module      : Main
Description : Turns a string into the TikZ code of the corresponding willow.
Copyright   : Gregor Feierabend
License     : GNU General Public License v3 (GPLv3)
Maintainer  : Gregor Feierabend
Stability   : experimental
Portability : POSIX

WillowTikZ turns a string into the TikZ code of the corresponding willow; its
main purpose is to showcase an application of a functorial parser.
-}

module Main where

import Prelude
import System.IO
import System.Environment
import Control.Monad
import Control.Applicative
import Data.Char
import Willows

-- |Converts a natural number, as defined in the `Willows` module into a
-- Haskell built-in `Integer`; this is only of interest for displaying natural
-- numbers more conveniently.
natToInteger ::    Nat     -- ^The `Nat` to be converted.
                -> Integer -- ^The resulting `Integer`
natToInteger O = 0
natToInteger (S n) = (natToInteger n) + 1

instance Show Nat where show n = show (natToInteger n)
deriving instance Eq Nat
deriving instance Show Tree
deriving instance Show Willow

-- |The functorial parse-function, turning strings into willows.
parse ::    [Char] -- ^The string to be parsed.
         -> Willow -- ^The resulting willow.
parse xs = foldl wcat (W (T "" []) O O) (map f xs)
  where f x = case x of
              '[' -> W (T "[" [T "" []]) O (S O)
              ']' -> W (T "]" [T "" []]) (S O) O
              _   -> W (T [x] []) O O

-- |Prints a minus symbol (respectively a plus symbol) at the `nL`-th
-- (respectively the `nR`-th) node on the leftmost (respectively the rightmost)
-- branch of the willow.
pointsOfAttachment ::    Nat    -- ^Depth of the currently processed node.
                      -> Nat    -- ^Parameter specifying a node at depth `nL`.
                      -> Nat    -- ^Parameter specifying a node at depth `nR`.
                      -> Bool   -- ^Indicates whether the node is leftmost.
                      -> Bool   -- ^Indicates whether the node's parent is
                                --  rightmost.
                      -> [Tree] -- ^The siblings to the right of the current
                                --  node.
                      -> [Char] -- ^LaTeX code for the minus (or plus) symbol.
pointsOfAttachment depth nL nR leftmost parentRightmost ts =
  (if depth == nL then minusSymbol leftmost else "")
   ++ (if depth == nR then plusSymbol parentRightmost ts else "")
  where
   minusSymbol leftmost          = if leftmost
                                   then "\\willowplus"
                                   else ""
   plusSymbol parentRightmost ts = if parentRightmost && Prelude.null ts
                                   then "\\willowminus"
                                   else ""

-- |Intended to break a label into multiple lines; currently only truncating
-- it after 10 characters and replacing spaces with a placeholder character.
breaklabel ::    [Char] -- ^Input string.
              -> [Char] -- ^Output string to recurs over.
              -> Int    -- ^Current position in the string.
              -> [Char] -- ^Actual output string.
breaklabel [] out _       = out
breaklabel _ out 10       = out -- breaklabel xs (out ++ "\\\\") 1
breaklabel (' ':xs) out n = breaklabel xs (out ++ "{\\textvisiblespace}")
                             (n + 1)
breaklabel (x:xs) out n   = breaklabel xs (out ++ [x]) (n + 1)

-- |Prints a willow in tikz-qtree format.
printnodes ::    Willow -- ^The willow to be printed.
              -> [Char] -- ^The resulting tikz-qtree.
printnodes (W TEmpty nL nR) = ""
printnodes (W (T label (t:ts)) nL nR) =
  "\\Tree [.\\node[fill=black](root){\\willowlabel{" ++ (breaklabel label [] 1)
    ++ "}" ++ (pointsOfAttachment O nL nR True True []) ++ "};"
    ++ (pnRec (t:ts) (S O) nL nR True True) ++ "]"

-- |Recursive helper function for `printnodes`.
pnRec ::    [Tree] -- ^The tree underlying the willow to be printed.
         -> Nat    -- ^The depth of the currently processed node.
         -> Nat    -- ^Paramenter specifying a leftmost node at depth `nL`.
         -> Nat    -- ^Paramenter specifying a rightmost node at depth `nR`.
        -> Bool    -- ^Indicates whether the node is leftmost.
        -> Bool    -- ^Indicates whether the node's parent is rightmost.
         -> [Char] -- ^The resulting tikz-qtree.
pnRec [] _ _ _ _ _ = ""
pnRec ((T label children):ts) depth nL nR leftmost parentRightmost =
  " [.{\\willowlabel{" ++ (breaklabel label [] 1) ++ "}"
   ++ (pointsOfAttachment depth nL nR leftmost parentRightmost ts) ++ "}"
   ++ (pnRec children (S depth) nL nR leftmost
       (parentRightmost && (Prelude.null ts))) ++ " ]"
   ++ (pnRec ts depth nL nR False parentRightmost)

-- |Determines whether the input string contains only legal characters.
isLegalString ::    [Char] -- ^The input string.
                 -> Bool   -- ^Result.
isLegalString [] = True
isLegalString (x:xs) = if (elem x ['+', '-', '=', '.', ',', ';', ' ', '[', ']']
                           || isAlphaNum x)
                       then isLegalString xs
                       else False

-- |The main function.
main :: IO ()
main = do
  args <- getArgs
  guard ((length args) == 1) <|>
   fail "You must provide exactly one input string as arguent."
  guard (isLegalString (args !! 0)) <|>
   fail "You may only use alpha-numeric characters or any of the following symbols: ['+', '-', '=', '.', ',', ';', ' ']"
  putStrLn "\\documentclass[border={30pt 30pt 30pt 10pt}]{standalone}"
  putStrLn "\\usepackage{willow}"
  putStrLn "\\begin{document}"
  putStrLn "\\begin{tikzpicture}[willow]\\setlength\\fboxsep{1pt}"
  putStrLn (printnodes $ parse (args !! 0))
  putStrLn "\\end{tikzpicture}"
  putStrLn "\\end{document}"
