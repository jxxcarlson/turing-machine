module Machine.EnumerateN where

import Turing (Table, Row(..), Operation(..), blank, initializeTape)

-- ENUMERATE THE NATURAL NUMBERS IN BINARY


-- Instuction Table for the machine
machine :: Table
machine = [
    Row { initial = "B", symbol = blank, operations = [P "0"], final = "I"}, 

    Row { initial = "I", symbol = "0", operations = [P "1"], final = "R"}, 
    Row { initial = "I", symbol = "1", operations = [P "0", L], final = "I"}, 
    Row { initial = "I", symbol = blank, operations = [P "1"], final = "R"}, 

    Row { initial = "R", symbol = blank, operations = [L], final = "I"}, 
    Row { initial = "R", symbol = "0", operations = [R], final = "R"}, 
    Row { initial = "R", symbol = "1", operations = [R], final = "R"}
   ]

-- Inital tape and configuration
initialTape = initializeTape []
initalConfiguration = ("B", initialTape)

