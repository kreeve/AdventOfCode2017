-- TODO Maybe use state monad?
import Util
import Control.Monad
import Data.Char
import qualified Data.Map as Map

data Register = Reg String Int deriving (Show, Eq)

data Identifier = RegName String | Lit Int deriving (Show, Eq)

data Instruction = Set String Identifier
  | Add String Identifier
  | Mod String Identifier
  | Mul String Identifier
  | Recv Identifier
  | Snd Identifier
  | Jgz Identifier Int deriving (Show, Eq)

type SymbolTable = Map.Map String Int

data ProgramState = ProgramState Int Int Int [Instruction] SymbolTable deriving (Show, Eq) -- PC, instructions, table

lookupReg :: String -> SymbolTable -> Int
lookupReg reg tbl =
  case Map.lookup reg tbl of
    Nothing -> 0
    Just v -> v

addToTable :: String -> Int -> SymbolTable -> SymbolTable
addToTable reg val tbl = Map.insert reg val tbl

getVal :: Identifier -> SymbolTable -> Int
getVal idnt tbl =
  case idnt of
    RegName r -> lookupReg r tbl
    Lit i -> i

update :: ProgramState -> ProgramState
update (ProgramState idx lastFreq recvd instrList tbl) =
  let
    instr = instrList !! idx
    tbl' = case instr of
      Add r i -> addToTable r ((lookupReg r tbl)+(getVal i tbl)) tbl
      Mul r i -> addToTable r ((lookupReg r tbl)*(getVal i tbl)) tbl
      Set r i -> addToTable r (getVal i tbl) tbl
      Mod r i -> addToTable r ((lookupReg r tbl) `mod` (getVal i tbl)) tbl
      _ -> tbl
    f' = case instr of      
      Snd x -> getVal x tbl
      _ -> lastFreq
    r' = case instr of
      Recv x -> if (getVal x tbl) > 0 then lastFreq else recvd
      _ -> recvd
    idx' = case instr of
      Jgz r offset -> if (getVal r tbl) > 0 then (idx+offset) else (idx+1)
      _ -> idx + 1
  in
    ProgramState idx' f' r' instrList tbl'

readIdent :: String -> Identifier
readIdent s = if (all (\x -> (isDigit x) || (x=='-')) s)
  then Lit (readInt s)
  else RegName s

readLine :: String -> Instruction
readLine line =
  let
    w = words line
    n = length w
    name = head w
  in
    case name of
      "set" -> Set (w !! 1) (readIdent (w !! 2))
      "add" -> Add (w !! 1) (readIdent (w !! 2))
      "mul" -> Mul (w !! 1) (readIdent (w !! 2))
      "jgz" -> Jgz (readIdent (w !! 1)) (readInt (w !! 2))
      "snd" -> Snd (readIdent (w !! 1))
      "rcv" -> Recv (readIdent (w !! 1))
      "mod" -> Mod (w !! 1) (readIdent (w !! 2))

part1 :: ProgramState -> Int
part1 state =
  let
    state' = update state
  in
    case state' of
      (ProgramState _ _ r _ _) -> if r > 0 then r else part1 state'

runMt :: ProgramState -> [Int] -> ProgramState
runMt (ProgramState idx lastFreq recvd instrList tbl) vals =
  let
    instr = instList !! idx
    


main :: IO()
main =
  do
    lines <- replicateM 41 getLine
    let
      instrs = map readLine lines
      start = ProgramState 0 0 0 instrs Map.empty
    print $ part1 start
