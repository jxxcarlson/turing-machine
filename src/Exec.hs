
module Exec where

import Turing (advanceN, showConfig, showConfig', (|>))
import Control.Monad
import Machine.EnumerateN



exec :: String -> IO ()
exec str = 
  case words str of
     [] -> putStrLn "??"
     (cmd:args) -> 
       case cmd of
         "help" -> help
         "run" -> run
         _ -> putStrLn "??"


help :: IO()
help = 
  do
    putStrLn ""
    putStrLn "  Commands"
    putStrLn "  ------------------------------------------------------------"
    putStrLn "  run                      run Turng machine (test example, 30 steps)"
    putStrLn ""
    putStrLn "  Type :quit to quit"
    putStrLn ""

run :: IO ()
run =
    let
        actions :: [IO()]
        actions = map (\n -> putStrLn $ (show n ++ ": " ++ show (advanceAndShow'  n)) ) [0..30]
    in
      do 
          sequence_ actions

advanceAndShow n = advanceN machine n initalConfiguration |> showConfig 30

advanceAndShow' n = advanceN machine n initalConfiguration |> showConfig' 30

