import Control.Monad
import Util
import qualified Data.Map as Map

data Dir = Up | Down deriving (Show,Eq)
data ScannerState = ScannerState Int Int Dir deriving (Show,Eq) -- Position, len

data Layer = Layer Int ScannerState deriving (Show, Eq) -- Depth, state

data PersonPos = Starting | PersonPos Layer Int | Done deriving (Show, Eq) -- Layer, position

data WorldState = World PersonPos [Layer] Int deriving (Show, Eq) -- Person, layers, cost so far

-- Update the state of a scanner at a timestep
updateScannerState :: ScannerState -> ScannerState
updateScannerState (ScannerState pos len dir) =
  if len == 1 then (ScannerState pos len dir)
  else
    case dir of
      Up -> if pos > 0
        then ScannerState (pos-1) len Up
        else ScannerState (pos+1) len Down
      Down -> if pos == len-1 
        then ScannerState (pos-1) len Up
        else ScannerState (pos+1) len Down
      
-- Cost of this person being here
cost :: PersonPos -> Int
cost (PersonPos (Layer depth (ScannerState spos len dir)) ppos) =
  if len == 1 then 0 else
    if spos == ppos
    then depth * len
    else 0
cost Starting = 0
cost Done = 0

-- Take one step in this world
step :: WorldState -> WorldState
step (World ppos layers c) =
  let
    layers' = map (\x -> (case x of
                      Layer dp state -> Layer dp (updateScannerState state))) layers
    ppos' = case ppos of
      Starting -> PersonPos (head layers) 0
      PersonPos (Layer depth state) pos ->
        if depth >= ((length layers) - 1)
        then Done
        else PersonPos (layers !! (depth+1)) pos
  in
    World ppos' layers' (c + (cost ppos'))
              
-- Create a state from mapping
createState :: Map.Map Int Int -> WorldState
createState dict = create' dict (World Starting [] 0)
  where
    create' d state =
      if length d <= 0
      then state
      else
        let
          ((next,v),d') = Map.deleteFindMin d
          layer = Layer next (ScannerState 0 v Down)
          state' = case state of
            World Starting [] c  -> World Starting ([layer]) c
            World p lst c -> World p (lst ++ [layer]) c
        in
          create' d' state'
    
-- Step completely
play :: WorldState -> Int
play state =
  case state of
  (World ps _ c) ->
    case ps of
      Done -> c
      _ -> play (step state)

makeDict :: [[String]] -> (Map.Map Int Int)
makeDict lines =
  let
    dict = Map.fromList (map make' lines)
      where
        make' line =
          let
            [k,v] = map readInt line
          in
            (k,v)
    allNums = take (foldr1 max (Map.keys dict)) [0,1..]
    dict' = foldr fix' dict allNums
      where
        fix' n d =
          let
            v = Map.lookup n d
          in
            case v of
              Nothing -> Map.insert n 1 d
              Just i -> Map.insert n i d
  in
    dict'
    
        

main :: IO()
main =
  do
    lines <- replicateM 43 getLine
    let
      vals = map (\l -> split ':' l) lines
      dict = makeDict vals
      start = createState dict
    print $ start
    print $ play start
