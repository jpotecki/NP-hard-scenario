module Main where

import Lib

main :: IO ()
main = do
    file <- readFile "robots.mat" >>= return.lines
    mapM_ calcPaths file
    