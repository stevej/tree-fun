module Data.Trees.KD1 where

import Data.List (sortBy, splitAt)
import Data.Ord (comparing)

type Point = [Int]

data KDTree = Branch Point KDTree KDTree | Leaf Point | Empty deriving (Show)

-- | building a KD tree from a list of Points.
--
-- >>> kd_from_list [[2,3], [5,4], [9,6], [4,7], [8,1], [7,2]]
-- Branch [7,2] (Branch [5,4] (Branch [2,3] Empty Empty) (Branch [4,7] Empty Empty)) (Branch [9,6] (Branch [8,1] Empty Empty) Empty)
kd_from_list :: [Point] -> KDTree
kd_from_list points = kd_from_list' points 0

kd_from_list' :: [Point] -> Int -> KDTree
kd_from_list' [] depth = Empty
kd_from_list' points depth = Branch median (kd_from_list' beginning_of_list (depth + 1)) (kd_from_list' end_of_list (depth + 1))
                            where
                              dimension = length $ head points -- guaranteed to not be empty due to pattern matching
                              sortFn = comparing (\xs -> xs !! (depth `mod` dimension))
                              sorted = sortBy sortFn points
                              middle_point = ((length sorted) `div` 2)
                              median = sorted !! middle_point
                              (beginning_of_list, xs) = splitAt middle_point sorted
                              end_of_list = tail xs


