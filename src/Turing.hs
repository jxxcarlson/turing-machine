module Turing where

someFunc :: IO ()
someFunc = putStrLn "someFunc"


type State = String
type Symbol = String
data Operation = L | R | P Symbol
type Tape = (Int, [Symbol])

data Row = Row {
      initial :: State,
      symbol :: Symbol,
      operations :: [Operation],
      final :: State
  } 

instance Show Operation where
   show op = case op of
        L -> "L"
        R -> "R"
        P s -> "P " ++ s

instance Show Row where 
  show (Row initial symbol operations final )= initial ++ ", " ++ symbol ++ ", " ++ show operations ++ " => " ++ final


showTable :: Table -> String
showTable table = 
    joinStrings "; " (fmap show table)

blank :: Symbol
blank = "_"


-- TABLE

type Table = [Row]


matchState :: State -> [Row] -> [Row]
matchState  state table  = 
    table |> filter (\row -> initial row == state)

matchSymbol :: Symbol -> [Row] ->  [Row]
matchSymbol   sym  table = 
    table |> filter (\row -> symbol row == sym || symbol row == "Any")


currentSymbol :: Tape -> Symbol
currentSymbol (n, symbols) = symbols !! n

advanceN :: Table -> Int -> (State, Tape) -> (State, Tape)
advanceN table n config =
    foldr (\_ acc -> advance table acc) config [1..n]

advance :: Table -> (State, Tape) -> (State, Tape)
advance table (state, tape) =
    let
        symbol = currentSymbol tape
        rows = table |> matchState state|> matchSymbol symbol
    in
        case rows of 
            [] -> (state, tape)
            (row:_) -> execute row (state, tape) 


execute :: Row -> (State, Tape) -> (State, Tape)
execute row (state, tape) =
    let
        state' = final row
        tape' = executeOperations (operations row) tape
    in
        (state', tape')

executeOperations :: [Operation] -> Tape -> Tape
executeOperations ops tape =
    foldr (\op acc -> executeOperation op acc) tape (reverse ops)       

executeOperation :: Operation -> Tape -> Tape
executeOperation op tape =
    case op of
        R -> right tape
        L -> left tape
        P s -> 
            let
                k = fst tape
            in
            if k /= 0
                then mapSecond (update k s) tape
                else mapSecond (update (k + 1) s) (prependBlank tape)
                

prependBlank :: Tape -> Tape
prependBlank (k, symbols) = (k+1, blank:symbols)

update :: Int -> a -> [a] -> [a]
update k a as = 
  let
    (first, (x:last)) = splitAt k as
   in
    first ++ a:last

mapFirst :: (a -> a) -> (a, b) -> (a, b)
mapFirst f (a,b) = (f a, b)

mapSecond :: (b -> b) -> (a, b) -> (a, b)
mapSecond f (a,b) = (a, f b)

--TAPE  
 
blankTape :: [Symbol]
blankTape = repeat blank

initializeTape :: [Symbol] -> Tape
initializeTape symbols = (0, symbols ++ blankTape)

initialSegment :: Int -> Tape -> Tape
initialSegment k (n, symbols) = (n, take k symbols)

showTape :: Int -> Tape -> String
showTape k tape =
    let 
        symbols = tape |> initialSegment k |> snd
        n = fst tape
        first = take n symbols  |> joinStrings ":"
        current = symbols !! n
        last = drop (n + 1) symbols |> joinStrings ":"
       
    in
        first ++ ":(" ++ current  ++ "." ++ show n ++ "):" ++ last

showTape' :: Int -> Tape -> String
showTape' k tape =
    tape 
      |> initialSegment k 
      |> snd
      |> fmap (\s -> if s == blank then " " else s)
      |> joinStrings ""


showConfig :: Int -> (State, Tape) -> (State, String)
showConfig n (state, tape) =
    (state, showTape n tape)

showConfig' :: Int -> (State, Tape) -> (State, Int, String)
showConfig' n (state, tape) =
    (state, (fst tape), showTape' n tape)    


left :: Tape -> Tape
left (0, symbols) = (0, blank:symbols)
left (k, symbols) = (k - 1, symbols)

right :: Tape -> Tape
right (k, symbols) = (k + 1, symbols)

testTape :: Tape
testTape = initializeTape ["A", "B", "C", "D"]



-- HELPERS

(|>) :: a -> (a -> b) -> b
(|>) a f = f a

joinStrings :: String -> [String] -> String
joinStrings _ [] = ""
joinStrings sep strings = 
    foldl (\str acc -> str ++ sep ++ acc) (head strings) (tail strings)


-- TEST DATA

a = advance m1
t0 = initializeTape []
c0 = ("b", t0)

a' = advance m2
t0' = initializeTape []
c0' = ("b", t0')


-- MACHINES


m1 :: Table
m1 = [
      Row { initial = "b", symbol = "Any", operations = [P "0", R], final = "c"}, 
      Row { initial = "c", symbol = "Any", operations = [R], final = "e"}, 
      Row { initial = "e", symbol = "Any", operations = [P "1", R], final = "f"}, 
      Row { initial = "f", symbol = "Any", operations = [R], final = "b"}
  ]


m2 :: Table
m2 = [
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


mAdd :: Table
mAdd = [
      Row { initial = "A", symbol = "1", operations = [P blank, R], final = "B"}, 
      Row { initial = "B", symbol = "1", operations = [R], final = "B"}, 
      Row { initial = "B", symbol = blank, operations = [P "1"], final = "C"}
  ]

tAdd = initializeTape ["1", "1", "1", blank, "1", "1", "1","1"]
cAdd = ("A", tAdd)


-- SQUARE ROOT


mList :: Table
mList = [
    Row { initial = "B", symbol = blank, operations = [P "0"], final = "I"}, 

    Row { initial = "I", symbol = "0", operations = [P "1"], final = "R"}, 
    Row { initial = "I", symbol = "1", operations = [P "0", L], final = "I"}, 
    Row { initial = "I", symbol = blank, operations = [P "1"], final = "R"}, 

    Row { initial = "R", symbol = blank, operations = [L], final = "I"}, 
    Row { initial = "R", symbol = "0", operations = [R], final = "R"}, 
    Row { initial = "R", symbol = "1", operations = [R], final = "R"}
   ]

tList = initializeTape []
cList= ("B", tList)

advanceList n = advanceN mList  n cList |> showConfig 30

advanceList' n = advanceN  mList n cList |> showConfig' 30

