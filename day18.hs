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
  | Jgz Identifier Identifier deriving (Show, Eq)

type SymbolTable = Map.Map String Int

data ProgramState =
  ProgramState {pc :: Int,
                fr :: Int,
                rc :: Int,
                vq:: [Int],
                sq:: [Int],
                prog :: [Instruction],
                table :: SymbolTable} deriving (Show, Eq) -- PC, instructions, table

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

updateTable :: Instruction -> SymbolTable -> SymbolTable
updateTable instr tbl =
  case instr of
      Add r i -> addToTable r ((lookupReg r tbl)+(getVal i tbl)) tbl
      Mul r i -> addToTable r ((lookupReg r tbl)*(getVal i tbl)) tbl
      Set r i -> addToTable r (getVal i tbl) tbl
      Mod r i -> addToTable r ((lookupReg r tbl) `mod` (getVal i tbl)) tbl
      _ -> tbl

updateValQ :: Instruction -> [Int] -> SymbolTable -> ([Int],SymbolTable)
updateValQ instr valQ tbl =
  let
    vq' = 
        case instr of
          (Recv r) -> case r of
            (RegName q) -> if (length valQ) > 0
              then tail valQ
              else valQ
            _ -> valQ
          _ -> valQ
    tbl' =
      case instr of
          (Recv r) -> case r of
            (RegName q) -> if (length valQ) > 0
              then addToTable q (head valQ) tbl
              else tbl
            _ -> tbl
          _ -> tbl
  in
    (vq', tbl')

updateSent :: Instruction -> [Int] -> SymbolTable -> [Int]
updateSent instr sent tbl = case instr of
  (Snd r) ->
    let
      v = getVal r tbl
    in
      sent ++ [v]
  _ -> sent

update :: ProgramState -> Bool -> ProgramState
update (ProgramState idx lastFreq recvd valQ sendQ instrList tbl) useQ =
  let
    instr = instrList !! idx
    tbl' = updateTable instr tbl
    f' = case instr of      
      Snd x -> getVal x tbl'
      _ -> lastFreq
    r' = case instr of
      Recv x -> if (getVal x tbl) /= 0 then lastFreq else recvd
      _ -> recvd
    (vq',tbl'') =
      if useQ
      then updateValQ instr valQ tbl'
      else (valQ,tbl')
    sent' =
      if useQ
      then updateSent instr sendQ tbl
      else sendQ
    idx' = case instr of
      Jgz r offset -> if (getVal r tbl) > 0 then (idx+ (getVal offset tbl)) else (idx+1)
      _ -> idx + 1
  in
    ProgramState idx' f' r' vq' sent' instrList tbl''

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
      "jgz" -> Jgz (readIdent (w !! 1)) (readIdent (w !! 2))
      "snd" -> Snd (readIdent (w !! 1))
      "rcv" -> Recv (readIdent (w !! 1))
      "mod" -> Mod (w !! 1) (readIdent (w !! 2))

part1 :: ProgramState -> Int
part1 state =
  let
    state' = update state False
    r = rc state'
  in
    if r > 0 then r else part1 state'

getSent :: ProgramState -> [Int]
getSent (ProgramState _ _ _ _ sq _ _) = sq

outOfBounds :: ProgramState -> Bool
outOfBounds state = (pc state) >= (length (prog state))

runMt :: ProgramState -> ProgramState
runMt state =
  if outOfBounds state then state
  else
  let
    state' = update state True
    instr = (prog state') !! (pc state')
    isRecv =
      case instr of
        (Recv r) -> True
        _ -> False
  in
    if (length (vq state')) == 0 && isRecv
    then state'
    else runMt state'

type MtState = (ProgramState, ProgramState)

-- Find a fixed point of a function, starting at `val`
fixedPointProg :: (MtState -> MtState) -> MtState -> Int -> Int
fixedPointProg func start v
  | start == val = v'
  | otherwise = fixedPointProg func val v'
  where
    val = func start
    p1 = fst val
    v' = v + length (sq p1)

runTwoThreads :: (ProgramState,ProgramState) -> (ProgramState, ProgramState)
runTwoThreads (s1,s2) =
  let
    s1' = runMt (s1 {sq=[]})
    vq' = sq s1
    s2' = s2 {vq = vq'}
    s2'' = runMt (s2' {sq=[]})
    vq2' = sq s2''
    s1'' = s1' {vq = vq2'}
  in
    (s1'', s2'')

main :: IO()
main =
  do
    lines <- replicateM 41 getLine
    let
      instrs = map readLine lines
      start = ProgramState 0 0 0 [] [] instrs Map.empty
    print $ instrs
    print $ part1 start
