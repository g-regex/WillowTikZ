import Prelude
import System.IO
import System.Environment
import Control.Monad
import Control.Applicative
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

printnodes :: Willow -> [Char]
printnodes (W TEmpty n1 n2) = ""
printnodes (W (T label (t:ts)) n1 n2) =
  "\\Tree [.\\node[fill=black](root){\\willowlabel{" ++ label
    ++ "}" ++ (charges O n1 n2 True True []) ++ "};"
    ++ (pn (t:ts) (S O) n1 n2 True True) ++ "]"

pn :: [Tree] -> Nat -> Nat -> Nat -> Bool -> Bool -> [Char]
pn [] depth n1 n2 leftmost rightmost = ""
pn ((T label chld):ts) depth n1 n2 leftmost rightmost =
  " [.{\\willowlabel{" ++ label ++ "}" ++ (charges depth n1 n2 leftmost rightmost ts) ++ "}"
    ++ (pn chld (S depth) n1 n2 leftmost (rightmost && (Prelude.null ts))) ++ " ]"
    ++ (pn ts depth n1 n2 False rightmost)

main :: IO ()
main = do
  args <- getArgs
  guard ((length args) == 1) <|> fail "You must provide exactly one input string as arguent."
  putStrLn "\\documentclass[border={30pt 30pt 30pt 10pt}]{standalone}"
  putStrLn "\\usepackage{willow}"
  putStrLn "\\begin{document}"
  putStrLn "\\begin{tikzpicture}[willow]\\setlength\\fboxsep{1pt}"
  putStrLn (printnodes $ parse (args !! 0))
  putStrLn "\\end{tikzpicture}"
  putStrLn "\\end{document}"
