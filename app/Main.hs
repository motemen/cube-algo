module Main where

import Lib

main :: IO ()
main = do
    s <- getLine
    putStrLn $ algToFacelets $ parseAlg s
