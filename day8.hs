import System.IO  
import Control.Monad
import Data.Char
import Data.List
import qualified Data.Set as Set
import Control.Monad.Fix

import Util


-- Constants to increment by
type Const = Int

-- One register w/ value
data Register = Reg String Int deriving (Show, Eq)

-- Condition
data Condition = Cond Register Operator Const deriving (Show, Eq)

-- Operators (comparison)
data Operator = GrtThan | LessThan | GrtOrEq | LessOrEq | EqTo | NotEqualTo deriving (Show, Eq)

-- Instructions
data Instr = Incr | Decr deriving (Show, Eq)

-- Change a register value in this mini language
data Changer = Change Register Instr Const deriving (Show, Eq)

-- Full expression (one line)
data Expression = Expr Changer Condition deriving (Show)

-- Symbol table
type SymbolTable = [(String, Register)]

-- Name of a register
regName :: Register -> String
regName (Reg name _) = name

-- Val of register
regVal :: Register -> Int
regVal (Reg _ val) = val

-- Update symbol table
symTabUpdate :: Register -> SymbolTable -> SymbolTable
symTabUpdate reg tbl =
  let
    curVal = lookup (regName reg) tbl
    newVal = ((regName reg), reg)
    tblWithout = case curVal of
      Just r -> remChar ((regName r),r) tbl
      Nothing -> tbl
  in
    tblWithout ++ [newVal]

-- Lookup in symbol table
symTabLookup :: String -> SymbolTable -> Register
symTabLookup name tbl =
  let
    val = lookup name tbl
  in
    case val of
      Just r -> r
      Nothing -> (Reg name 0)
      
-- Some parser subroutines
parseInstr :: String -> Instr
parseInstr "inc" = Incr
parseInstr "dec" = Decr

parseConst :: String -> Const
parseConst = read

parseOper :: String -> Operator
parseOper s = case s of
  "=" -> EqTo
  ">" -> GrtThan
  ">=" -> GrtOrEq
  "<" -> LessThan
  "<=" -> LessOrEq
  "!=" -> NotEqualTo
  _ -> EqTo

-- Parse a list of words into an expression
parseLine :: [String] -> SymbolTable -> Expression
parseLine [r1,inst,c,_,r2,op,c1] symTab =
  let
    reg1 = symTabLookup r1 symTab
    instr = parseInstr inst
    con = parseConst c
    reg2 = symTabLookup r2 symTab
    oper = parseOper op
    con1 = parseConst c1
    changePart = Change reg1 instr con
    condPart = Cond reg2 oper con1
  in
    Expr changePart condPart

-- Get the func that an operator is
operToFunc :: Operator -> (Int -> Int -> Bool)
operToFunc oper = case oper of
  LessThan -> (<)
  GrtThan -> (>)
  GrtOrEq -> (>=)
  LessOrEq -> (<=)
  EqTo -> (==)
  NotEqualTo -> (/=)

-- Convert instruction to func
instrToFunc :: Instr -> (Int -> Int -> Int)
instrToFunc inst = case inst of
  Incr -> (+)
  Decr -> (-)
  
-- Process condition
evalCond :: Condition -> Bool
evalCond (Cond reg oper const) = (operToFunc oper) (regVal reg) const

-- Execute an instruction
execInstr :: Changer -> Register
execInstr (Change reg inst const) =
  let
    f = instrToFunc inst
    v = f (regVal reg) const
  in
    Reg (regName reg) v

-- Execute a line
execLine :: [String] -> SymbolTable -> SymbolTable
execLine line symTab = 
  let
    expr = parseLine line symTab
    change = case expr of
      Expr ch cnd -> ch
    cond = case expr of
      Expr ch cnd -> cnd
    condTrue = evalCond cond
    res = case condTrue of
      True -> symTabUpdate (execInstr change) symTab
      False ->
        let
          reg1 = case change of
            Change reg _ _ -> reg
          reg2 = case cond of
            Cond reg _ _ -> reg
        in
          symTabUpdate reg1 (symTabUpdate reg2 symTab)
  in
    res

-- Run a bunch of lines, output a symtab
execProg :: [[String]] -> SymbolTable -> SymbolTable
execProg [] tbl = tbl
execProg (line:xs) tbl = execProg xs (execLine line tbl)

-- Compute part 1!!
part1 :: [[String]] -> Int
part1 lines =
  let
    tbl = execProg lines []
    vals = map (\x -> regVal (snd x)) tbl
  in
    foldr1 max vals

-- Part 2: Keep track as we go
part2 :: [[String]] -> Int
part2 lines =
  let
    idxs = take (length lines) [1,2..]
    allTabs = map (\n -> part1 (take n lines)) idxs
  in
    foldr1 max allTabs

main :: IO()
main =
  do
    lines <- replicateM 1000 getLine
    let
      vals = map words lines
    print $ part1 vals
    print $ part2 vals
