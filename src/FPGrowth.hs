module FPGrowth(genAllFreqPat) where

import Data.Bifunctor (Bifunctor (bimap))
import Data.Foldable (Foldable (foldl'))
import Data.List (nub, sortBy)
import Data.Map qualified as M
import Data.Maybe (fromJust)

-- transacrion
newtype Tx a = Tx {items :: [a]} deriving (Show)

-- item -> frequency
type ItemFreqMap a = M.Map a Int

-- fp tree node
data Node a = Node a Int (M.Map a (Node a)) deriving (Show)

-- generate frequency map for transactions
genFreq :: (Ord a) => [Tx a] -> ItemFreqMap a
genFreq = foldl' go M.empty . concatMap items
  where
    go m x = case M.lookup x m of
      Just i -> M.insert x (i + 1) m
      Nothing -> M.insert x 1 m

-- generate ordered item set
genOrdered :: (Ord a) => ItemFreqMap a -> Int -> [Tx a] -> [Tx a]
genOrdered m th = map (Tx . ordered . geth . indexed . items)
  where
    indexed = map (\it -> (it, fromJust (M.lookup it m)))
    geth = filter ((>= th) . snd) -- greater than threshold
    ordered = map fst . sortBy cmp
    -- compare on freq then alpha
    cmp (a1, f1) (a2, f2) = case compare f1 f2 of
      EQ -> compare a1 a2
      LT -> GT
      GT -> LT

-- make fp tree from ordered item set
mkFPTree :: (Ord a) => a -> [Tx a] -> Node a
mkFPTree root = foldl' (\n -> insert n . items) (Node root 0 M.empty)
  where
    insert n [] = n
    insert (Node n k cs) (it : tx)
      | n == it = insert (Node n (k + 1) cs) tx
      | otherwise = case M.lookup it cs of
          Just n' -> Node n k (M.insert it (insert n' (it : tx)) cs)
          Nothing -> Node n k (M.insert it (insert (Node it 1 M.empty) tx) cs)

-- generate fp tree from txs
genFPTree :: (Ord a) => a -> Int -> [Tx a] -> Node a
genFPTree root threshold txs = mkFPTree root ordtx
  where
    fm = genFreq txs -- frequency map
    ordtx = genOrdered fm threshold txs

-- preorder traversal
preorder :: Node a -> [[(a, Int)]]
preorder (Node n k cs)
  | M.null cs = [[(n, k)]]
  | otherwise = map ((n, k) :) $ concatMap (preorder . snd) (M.toList cs)

-- generate conditional pattern base
condPatBase :: ( Eq a) =>a -> Node a -> [([a], Int)]
condPatBase it (Node _ _ ns) = nub . filter ((/= []) . fst) $ map (path []) prefixes
  where
    prefixes = concatMap (preorder . snd) (M.toList ns)
    path _ [] = ([], -1) -- empty prefix
    path acc ((tx, k) : txs)
      | tx == it = (reverse acc, k)
      | otherwise = path (tx : acc) txs

-- generate conditional frequent pattern tree
condPatTree :: ( Eq a) =>Int -> [([a], Int)] -> [(a, Int)]
condPatTree th cpb = filter ((>= th) . snd) [(it, support it) | it <- its]
  where
    its = nub $ concatMap fst cpb
    support it = sum [i | (xs, i) <- cpb, it `elem` xs]

-- generate frequent pattern for it -> cpt
freqPat :: ( Eq a) =>a -> [(a, Int)] -> [([a], Int)]
freqPat it cpt = map (bimap (it :) minimum . unzip) $ filter (/= []) (ps cpt)
  where
    -- powerset
    ps [] = [[]]
    ps (x : xs) = [x : p | p <- ps xs] ++ ps xs

-- generate all frequent patterns
genAllFreqPat :: (Ord a) => a -> Int -> [Tx a] -> M.Map a [([a], Int)]
genAllFreqPat root th txs = M.fromList $ map (\i -> (i, fp i tree)) fis
  where
    -- frequent items
    fis = map fst . filter ((>= th) . snd) . M.toList . genFreq $ txs
    -- fp tree
    tree = genFPTree root th txs
    -- frequent pattern from i and tree
    fp i = freqPat i . condPatTree th . condPatBase i
