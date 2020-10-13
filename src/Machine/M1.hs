module Machine.M1 where 


import Turing (Table, Row(..), Operation(..), blank)


machine :: Table
machine = [
      Row { initial = "b", symbol = "Any", operations = [P "0", R], final = "c"}, 
      Row { initial = "c", symbol = "Any", operations = [R], final = "e"}, 
      Row { initial = "e", symbol = "Any", operations = [P "1", R], final = "f"}, 
      Row { initial = "f", symbol = "Any", operations = [R], final = "b"}
  ]

