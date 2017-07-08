module Main
where

import Http.Headers

main = do
    putStrLn $ show $ map parseType ["GET", "POST", "wrong"]
