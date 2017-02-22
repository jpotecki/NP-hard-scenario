module Main where

import Lib

main :: IO ()
main = do
    file <- readFile "robots.mat" >>= return.lines
    calcPaths $ file !! 19


    