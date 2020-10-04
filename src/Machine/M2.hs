module Machine.M2 where

import Turing


machine :: Table
mmachine = [
      Row { initial = "b", symbol = "Any", operations = [P "E", R, P "E", R, P "0", R, R, P "0", L, L], final = "o"}, 
     
      Row { initial = "o", symbol = "1", operations = [R, P "x", L, L, L], final = "o"}, 
      Row { initial = "o", symbol = "0", operations = [], final = "q"}, 
     
      Row { initial = "q", symbol = "0", operations = [R, R], final = "q"}, 
      Row { initial = "q", symbol = "1", operations = [R, R], final = "q"}, 
      Row { initial = "q", symbol = blank, operations = [P "1", L], final = "p"}, 

      Row { initial = "p", symbol = "x", operations = [P blank, R], final = "q"},
      Row { initial = "p", symbol = "E", operations = [R], final = "f"},
      Row { initial = "p", symbol = blank, operations = [L, L], final = "p"},

      Row { initial = "f", symbol = "Any", operations = [R,R], final = "f"},
      Row { initial = "f", symbol = blank, operations = [P "0", L, L], final = "o"}

  ]
