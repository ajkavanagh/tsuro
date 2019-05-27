module Tile where
    {-( -}
    {-, cards-}
    {-) where-}

import Data.List (tails, intercalate, sortBy, nub, sort)
import Data.List.Split (chunksOf)
import Control.Lens (over, both)
import Flow


uniqueTiles :: Integral a => [[a]]
uniqueTiles = filteredTiles ps
  where ps = generateAllTiles pairs

filteredTiles :: Integral a => [[a]] -> [[a]]
filteredTiles [] = []
filteredTiles [x] = [x]  -- optimisation to avoid checking an empty list
filteredTiles (x:xs) = x : filteredTiles (filter f xs)
  where f n = n `notElem` rotatedTilesSet x


rotateTile :: Integral a => [a] -> [a]
rotateTile xs = map rot xs |> chunksOf 2 |> map sort |> sort |> concat
  where rot a = (a+2) `mod` 8

-- Represents a set of tiles in all rotations of the same tile.
-- Note that it may be 1 tile, or up to 4 rotations of that tile.
rotatedTilesSet :: Integral a => [a] -> [[a]]
rotatedTilesSet xs = iterate rotateTile xs
                  |> take 4
                  |> sort
                  |> nub


-- | paths a list of 2-lists where each (a,b) is a path on the
-- tile.  These are then combined to make all of the possible
-- cards
pairs :: Num a => [[a]]  -- [[1,2],[3,4],[5,6],[7,8]]
pairs = comb 2 [0,1,2,3,4,5,6,7]


-- generates to the tile format which is [a] with length 8
generateAllTiles :: Eq a => [[a]] -> [[a]]
generateAllTiles ps = expandSegments [] ps
                   |> chunksOf 4
                   |> map concat

showTile :: Show a => [a] -> String
showTile xs = intercalate "-" $ map go $ chunksOf 2 xs
  where go [x,y] = show x <> show y


-- | expandSegments recusively builds from the second list to the first lst by
-- using the segments function. So it takes a list of [[a1,a2], [b1,b2],
-- [c1,c2]..] into all the possible combinations of sequences of pairs that
-- contain the unique numbers 0-7.
expandSegments :: Eq a => [[a]] -> [[a]] -> [[a]]
expandSegments xs [] = xs
expandSegments xs ps =
    concatMap (uncurry expandSegments) $ segments xs ps


-- | segments take two lists and returns a list of pairs where the first element
-- of the pair is the first list with a element added from the second list.
segments :: Eq a => [[a]] -> [[a]] -> [([[a]], [[a]])]
segments xs ps = map (\x -> segment (xs ++ [x],ps)) $ selectSet ps

-- | segment takes a pair of lists of pairs (lists of 2 items) and filters out
-- the second pair of lists such that they don't contain any of the first items.
segment :: Eq a => ([[a]], [[a]]) -> ([[a]],[[a]])
segment (ps,pps) = (ps, notIn ps pps)


-- return a [[0,1],[0,2],[0,3]..] where the first element in the
-- embedded list is the same.
selectSet :: Eq a => [[a]] -> [[a]]
selectSet xs@((a:_):_) = takeWhile ((==a).head) xs
selectSet [] = []


notIn :: Eq a => [[a]] -> [[a]] -> [[a]]
notIn xs = filter (\(x:y:_) -> x `notElem` zs && y `notElem` zs)
  where zs = concat xs


-- | this pair of functions (comb, inner) generates all the unique
-- lists of a list of lenght n.
comb :: Int -> [a] -> [[a]]
comb 0 _ = [[]]
comb n xs = concatMap (inner n) $ tails xs

inner _ [] = []
inner n (y:xs') = map (y:) $ comb (n-1) xs'
