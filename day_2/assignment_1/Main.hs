module Main (main) where

main :: IO ()
main = do
    spreadsheet <- map (map read . words) . lines <$> getContents :: IO [[Int]]

    let maxs = map maximum spreadsheet
        mins = map minimum spreadsheet
        diff = zipWith (-) maxs mins

    print $ sum diff
