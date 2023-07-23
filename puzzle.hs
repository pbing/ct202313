import Data.List

digrams :: [String]
digrams = ["NT", "OR", "MA", "SE", "IK", "NS", "PH"]

combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [[]]
combinations n xs = [y:ys | y:xs' <- tails xs
                          , ys <- combinations (n-1) xs']

-- 840 solutions
solve :: [String]
solve = concat <$> concatMap permutations (combinations 4 digrams)
