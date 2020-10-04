module Machine.Add where

import Turing


machine :: Table
machine = [
      Row { initial = "A", symbol = "1", operations = [P blank, R], final = "B"}, 
      Row { initial = "B", symbol = "1", operations = [R], final = "B"}, 
      Row { initial = "B", symbol = blank, operations = [P "1"], final = "C"}
  ]

initialTape = initializeTape ["1", "1", "1", blank, "1", "1", "1","1"]
initialConfiguration = ("A", tAdd)
