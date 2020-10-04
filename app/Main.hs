module Main where

import Turing
import Control.Monad
import Data.Drinkery.Combinators

main :: IO ()
main =
    do 
      drainFrom $ forM [0..100] (\n -> do putStrLn $ (show n ++ ": " ++ show (advanceList'  n)) )
      -- putStrLn $  show $ advanceList'  100                                              