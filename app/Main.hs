module Main where

import Turing (advanceN, showConfig, showConfig', (|>))
import Control.Monad
import Data.Drinkery.Combinators
import Machine.EnumerateN


main :: IO ()
main =
    do 
      drainFrom $ forM [0..100] (\n -> do putStrLn $ (show n ++ ": " ++ show (advanceAndShow'  n)) )


advanceAndShow n = advanceN machine  n initalConfiguration |> showConfig 30

advanceAndShow' n = advanceN  machine n initalConfiguration |> showConfig' 30

