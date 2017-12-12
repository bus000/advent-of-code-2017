module Main (main) where

main :: IO ()
main = do
    spreadsheet <- map (map read . words) . lines <$> getContents

    let pairs = concatMap getPairs spreadsheet
        mod0Pairs = filter (\(x, y) -> x `mod` y == 0) pairs

    print $ (sum . map (uncurry div)) mod0Pairs

getPairs :: [Int] -> [(Int, Int)]
getPairs xs = filter (uncurry (/=)) $ (,) <$> xs <*> xs
