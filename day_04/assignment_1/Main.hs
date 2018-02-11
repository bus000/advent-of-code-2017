{- A new system policy has been put in place that requires all accounts to use a
 - passphrase instead of simply a password. A passphrase consists of a series of
 - words (lowercase letters) separated by spaces.
 -
 - To ensure security, a valid passphrase must contain no duplicate words.
 -
 - For example:
 -
 - * aa bb cc dd ee is valid.
 - * aa bb cc dd aa is not valid - the word aa appears more than once.
 - * aa bb cc dd aaa is valid - aa and aaa count as different words.
 -
 - The system's full passphrase list is available as your puzzle input. How many
 - passphrases are valid? -}
module Main (main) where

import Data.List (group, sort)

main :: IO ()
main = do
    valid <- filter validpass . lines <$> getContents

    print $ length valid

validpass :: String -> Bool
validpass = not . any (> 1) . map length . group . sort . words
