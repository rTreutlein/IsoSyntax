module Main where

import Lib

main :: IO ()
main = do
    putStrLn "Please Input some Lojban to Translate"
    input <- getLine
    pure ()
