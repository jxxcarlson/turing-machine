module Turing (advanceN, Table, Row(..), Operation(..), blank, initializeTape, showConfig, showConfig', (|>))
 where

someFunc :: IO ()
someFunc = putStrLn "someFunc"


type State = String
type Symbol = String

-- Move left or right, or print a symbol
data Operation = L | R | P Symbol 

-- Int is the location of the reading head
-- The tap is implemented as being infnite on the right,
-- but we add blank cells on the left as needed, so
-- that it is effectively two-dimensional
type Tape = (Int, [Symbol])


-- A row in the table of "machine instructions"
data Row = Row {
      initial :: State,
      symbol :: Symbol,
      operations :: [Operation],
      final :: State
  } 

type Table = [Row]


-- Show stuff

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




-- OPERATINNG THE MACHINE

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
                


--TAPE  
 
blankTape :: [Symbol]
blankTape = repeat blank

initializeTape :: [Symbol] -> Tape
initializeTape symbols = (0, symbols ++ blankTape)

-- Use this to pretend that the tape is infinite
-- in both directions
prependBlank :: Tape -> Tape
prependBlank (k, symbols) = (k+1, blank:symbols)

left :: Tape -> Tape
left (0, symbols) = (0, blank:symbols)
left (k, symbols) = (k - 1, symbols)

right :: Tape -> Tape
right (k, symbols) = (k + 1, symbols)


-- HELPERS

(|>) :: a -> (a -> b) -> b
(|>) a f = f a

joinStrings :: String -> [String] -> String
joinStrings _ [] = ""
joinStrings sep strings = 
    foldl (\str acc -> str ++ sep ++ acc) (head strings) (tail strings)

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


