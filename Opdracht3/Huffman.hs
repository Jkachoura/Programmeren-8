module Huffman where
    import Data.List
    import Data.Function
    import Data.Ord
    import Control.Arrow
    import qualified Data.Map as M

    data HuffmanTree = Leaf Char Int
                     | Fork HuffmanTree HuffmanTree Int
                     deriving (Show, Read)
    
    type Codemap a = M.Map Char [a]

    class Eq a => Bit a where
        zer :: a
        one :: a

    instance Bit Int where
        zer = 0
        one = 1
    
    gewicht::HuffmanTree -> Int
    gewicht (Leaf _ g)   = g
    gewicht (Fork _ _ g) = g

    maakFork::HuffmanTree -> HuffmanTree -> HuffmanTree
    maakFork t1 t2 = Fork t1 t2 (gewicht t1 + gewicht t2)

    charHoeveelheid::String -> [(Char, Int)]
    charHoeveelheid str = map (\x -> (head x, length x)) grouped
        where
            grouped = group (sort str)
    
    maakTree::[(Char, Int)] -> HuffmanTree
    maakTree = bld . map (uncurry Leaf) . sortBy (compare `on` snd)
        where   bld (t:[]) = t
                bld (l:r:cs)  = bld $ insertBy (compare `on` gewicht) (maakFork l r) cs

    maakCodemap :: Bit a => HuffmanTree -> Codemap a
    maakCodemap = M.fromList . maakCodelist
        where   maakCodelist (Leaf c w)    = [(c, [])]
                maakCodelist (Fork l r w)  = map (addBit zer) (maakCodelist l) ++ map (addBit one) (maakCodelist r)
                    where addBit b = second (b :)
    
    stringTree:: String -> HuffmanTree
    stringTree = maakTree . charHoeveelheid

    stringCodemap:: Bit a => String -> Codemap a
    stringCodemap = maakCodemap . stringTree