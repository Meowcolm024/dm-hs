module Apriori(apriori, aprioriTraced) where

import Control.Monad (unless, when)
import Control.Monad.State qualified as MS
import Data.Foldable (Foldable (foldl'))
import Data.IORef qualified as IORef
import Data.Map qualified as M
import Data.Set qualified as S

type ItemSet a = S.Set a

newtype Tx a = Tx {items :: [a]} deriving (Show)

-- generate large 1 itemsets
genL1 :: (Ord a) => Int -> [Tx a] -> S.Set (ItemSet a)
genL1 sp =
  S.fromList
    . map (S.singleton . fst)
    . filter ((>= sp) . snd)
    . M.toList
    . foldl' go M.empty
    . concatMap items
  where
    go acc it = case M.lookup it acc of
      Just i -> M.insert it (i + 1) acc
      Nothing -> M.insert it 1 acc

-- subset of size k-1
subset1 :: (Ord a) => S.Set a -> [S.Set a]
subset1 st =
  map S.fromList
    . filter (\s -> length s == S.size st - 1)
    . subsets
    . S.toList
    $ st
  where
    subsets [] = [[]]
    subsets (x : xs) = subsets xs ++ map (x :) (subsets xs)

-- candidate generation
candidateGen :: (Eq a, Ord a) => S.Set (ItemSet a) -> S.Set (ItemSet a)
candidateGen its = S.fromList pruned
  where
    itss = S.toList its
    -- join step
    joined = [p `S.union` q | p <- itss, q <- itss, p /= q]
    -- prune step
    pruned = filter (all (`S.member` its) . subset1) joined

-- generate large itemset from candidate (counting step)
largeGen :: (Ord a) => Int -> [Tx a] -> S.Set (ItemSet a) -> S.Set (ItemSet a)
largeGen th txs = S.fromList . filter large . S.toList
  where
    txss = map (S.fromList . items) txs
    large its = length (filter (S.isSubsetOf its) txss) >= th

-- one iteration of apriori
apriori' :: (Ord a) => Int -> [Tx a] -> S.Set (ItemSet a) -> S.Set (ItemSet a)
apriori' th txs = largeGen th txs . candidateGen

-- apriori to gen all large itemset
apriori :: (Ord a) => Int -> [Tx a] -> S.Set (ItemSet a)
apriori th txs = MS.execState loop (genL1 th txs)
  where
    loop = do
      its <- MS.get
      let its' = its `S.union` apriori' th txs its
      MS.put its'
      -- loop until no new itemset generated
      when (S.size its' > S.size its) loop

showItemSet :: (Show a) => S.Set (ItemSet a) -> IO ()
showItemSet its = print $ map S.toList (S.toList its)

-- traced item set generation
aprioriTraced :: (Show a, Ord a) => Int -> [Tx a] -> IO ()
aprioriTraced th txs = do
  let l1 = genL1 th txs
  putStrLn "------ large 1"
  showItemSet l1
  putStrLn "------\n"
  IORef.newIORef l1 >>= loop 2
  where
    loop i ref = do
      its <- IORef.readIORef ref
      putStrLn $ "------ candidate " <> show i
      let can = candidateGen its
      showItemSet can
      putStrLn $ "------ large " <> show i
      let its' = largeGen th txs can
      showItemSet its'
      putStrLn "------\n"
      IORef.writeIORef ref its'
      unless (S.null its') $ loop (i + 1) ref
