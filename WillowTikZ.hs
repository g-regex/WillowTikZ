import Prelude
import System.IO
import System.Environment
import Control.Monad
import Control.Applicative
import Data.Char
import Willows

natToInteger :: Nat -> Integer
natToInteger O = 0
natToInteger (S n) = (natToInteger n) + 1

instance Show Nat where show n = show (natToInteger n)
deriving instance Eq Nat
deriving instance Show Tree
deriving instance Show Willow

parse :: [Char] -> Willow
parse xs = foldl wcat (W (T "" []) O O) (map f xs)
  where f x = case x of
              '[' -> W (T "[" [T "" []]) O (S O)
              ']' -> W (T "]" [T "" []]) (S O) O
              _   -> W (T [x] []) O O

charges :: Nat -> Nat -> Nat -> Bool -> Bool -> [Tree] -> [Char]
charges depth n1 n2 leftmost rightmost ts =
  (if depth == n1 then omin leftmost else "")
    ++ (if depth == n2 then oplu rightmost ts else "")
    where
      omin leftmost = if leftmost then "\\willowplus" else ""
      oplu rightmost ts = if rightmost && Prelude.null ts then "\\willowminus" else ""

breaklabel :: [Char] -> [Char] -> Int -> [Char]
breaklabel [] out _ = out
breaklabel xs out 10 =  out -- breaklabel xs (out ++ "\\\\") 1
breaklabel (' ':xs) out n =  breaklabel xs (out ++ "{\\textvisiblespace}") (n + 1)
breaklabel (x:xs) out n =  breaklabel xs (out ++ [x]) (n + 1)

printnodes :: Willow -> [Char]
printnodes (W TEmpty n1 n2) = ""
printnodes (W (T label (t:ts)) n1 n2) =
  "\\Tree [.\\node[fill=black](root){\\willowlabel{" ++ (breaklabel label [] 1)
    ++ "}" ++ (charges O n1 n2 True True []) ++ "};"
    ++ (pn (t:ts) (S O) n1 n2 True True) ++ "]"

pn :: [Tree] -> Nat -> Nat -> Nat -> Bool -> Bool -> [Char]
pn [] depth n1 n2 leftmost rightmost = ""
pn ((T label chld):ts) depth n1 n2 leftmost rightmost =
  " [.{\\willowlabel{" ++ (breaklabel label [] 1) ++ "}"
    ++ (charges depth n1 n2 leftmost rightmost ts) ++ "}"
    ++ (pn chld (S depth) n1 n2 leftmost (rightmost && (Prelude.null ts))) ++ " ]"
    ++ (pn ts depth n1 n2 False rightmost)

isLegalString :: [Char] -> Bool
isLegalString [] = True
isLegalString (x:xs) = if (elem x ['+', '-', '=', '.', ',', ';', ' ', '[', ']']
                            || isAlphaNum x)
                       then isLegalString xs
                       else False

main :: IO ()
main = do
  args <- getArgs
  guard ((length args) == 1) <|> fail "You must provide exactly one input string as arguent."
  guard (isLegalString (args !! 0)) <|> fail "You may only use alpha-numeric characters or any of the following symbols: ['+', '-', '=', '.', ',', ';', ' ']"
  putStrLn "\\documentclass[border={30pt 30pt 30pt 10pt}]{standalone}"
  putStrLn "\\usepackage{willow}"
  putStrLn "\\begin{document}"
  putStrLn "\\begin{tikzpicture}[willow]\\setlength\\fboxsep{1pt}"
  putStrLn (printnodes $ parse (args !! 0))
  putStrLn "\\end{tikzpicture}"
  putStrLn "\\end{document}"
