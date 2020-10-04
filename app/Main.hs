module Main where

import Turing (advanceN, showConfig, showConfig', (|>))
import Control.Monad
import Data.Drinkery.Combinators
import Machine.EnumerateN


main :: IO ()
main =
    let
        actions :: [IO()]
        actions = map (\n -> putStrLn $ (show n ++ ": " ++ show (advanceAndShow'  n)) ) [0..100]
    in
      do 
          sequence_ actions

advanceAndShow n = advanceN machine n initalConfiguration |> showConfig 30

advanceAndShow' n = advanceN machine n initalConfiguration |> showConfig' 30

