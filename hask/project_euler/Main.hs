module Main where
import Data.Map (Map)
import qualified Data.Map as Map

sieve xs = sieve1 xs Map.empty
  where
    sieve1 [] table = []
    sieve1 (x:xs) table =      case Map.lookup x table of
        Nothing −> x : sieve1 xs (Map.insert (x*x) [x] table)
        Just facts −> sieve1 xs (foldl reinsert (Map.delete x table) facts)
      where
        reinsert table prime = Map.insertWith (++) (x+prime) [prime] table

