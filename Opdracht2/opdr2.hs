import System.Environment
import Data.Char

-- Opdracht 1
data BinTree a = Empty 
                | Node a (BinTree a) (BinTree a)
                deriving (Show, Read)

-- Opdracht 2
-- Push an element of type a on the binary tree
push::(Ord a) => (BinTree a) -> a -> (BinTree a)
push Empty x = Node x Empty Empty
push (Node n left right) x
                          | x < n     = Node n (push left x) right
                          | otherwise = Node n left (push right x)

-- Push a list with elements of type a on the binary tree
pushlist::(Ord a) => (BinTree a) -> [a] -> (BinTree a)
pushlist tree []     = tree
pushlist tree (x:xs) = pushlist (push tree x) xs

-- Map a function to all elements in a tree
maptree::(a -> b) -> (BinTree a) -> (BinTree b)
maptree _ Empty                 = Empty
maptree f (Node a l r)          =  Node (f a) (maptree f l) (maptree f r)

-- Return list with the filtered elements of the tree
filtertree::(a -> Bool) -> (BinTree a) -> [a]
filtertree f Empty                        = []
filtertree f (Node x l r)   
                            | f x         = x : filtertree f l ++ filtertree f r
                            | otherwise   = filtertree f l ++ filtertree f r

-- Apply preorder traversal on a tree
preorder::(BinTree a) -> [a]
preorder Empty        = []
preorder (Node x l r) = [x] ++ preorder l ++ preorder r

-- Apply postorder traversal on a tree
postorder::(BinTree a) -> [a]
postorder Empty        = []
postorder (Node x l r) = postorder l ++ postorder r ++ [x]

-- Apply inorder traversal on a tree
inorder::(BinTree a) -> [a]
inorder Empty        = []
inorder (Node x l r) = inorder l ++ [x] ++ inorder r

main::IO()
main = do
  args <- getArgs
  case args of
    [inputFile, outputFile] -> do
      input <- readFile inputFile
      -- Inputfile wordt omgezet in String
      let y = input :: String
      -- De string wordt gepushed op een empty BinTree
      let tree = pushlist Empty y
      -- Alle characters in de BinTree worden omgezet in Int
      let inttree = maptree (ord) tree
      -- De BinTree wordt omgezet in een String
      let stree = show inttree
      -- De String wordt geschreven naar een Outputfile
      writeFile outputFile stree
      -- Daarna wordt de gegenereerde file weer ingelezen
      reading <- readFile outputFile
      -- Met de read functie wordt het omgezet in een BinTree
      let x = read reading
      -- De BinTree dat bestaat uit Ints wordt omgezet in Characters
      let mtree = maptree (chr) x
      -- Los een inorder traveral uit op de BinTree
      let inorderx = inorder mtree
      -- Print het op de scherm
      putStrLn inorderx
      -- Haal vervolgens alleen de cijfers eruit
      let filtree = filtertree (isDigit) mtree
      -- Print het op de scherm
      putStrLn filtree
      putStrLn "done..."
    _ -> putStrLn "Usage: <input_file> <output_file>"