import Data.Map (Map, elems)
import qualified Data.Map as Map
import Control.Monad.Identity
import Control.Monad.Trans.Reader
import Text.ParserCombinators.Parsec
import Numeric (showHex, showIntAtBase)
import Data.Char (intToDigit)

import Data.Char  (digitToInt)
import Data.Maybe (listToMaybe, fromJust)
import Numeric    (readInt)
import Data.Bits

type Addr = Int

type Memory = Map Int Int

data Operation = SetMask Mask | WriteMem Addr Int deriving (Show)

type MachineState = (Memory, Mask)

type WriteMemOp = Addr -> Int -> ReaderT MachineState Identity MachineState

data Mask = Mask
  { mask :: Int,
    override :: Int
  }

readBin :: String -> Maybe Int
readBin = fmap fst . listToMaybe . readInt 2 (`elem` "01") digitToInt
-- readBin "1001" == Just 9

showAsBinary :: Int -> String
showAsBinary x =
  let value = showIntAtBase 2 intToDigit x ""
      suffixLen = 36 - length value
  in replicate  suffixLen '0' ++ value

instance Show Mask where
   show (Mask mask' override') =
       "Mask {mask = " ++ showAsBinary mask' ++ ", override = " ++ showAsBinary override' ++ "}"

startsWith :: String -> String -> Bool
startsWith str start = all (==True) $ zipWith (==) str start

parseMaskFromString :: String -> Mask
parseMaskFromString str =
  let mask' = map (\c -> if c == 'X' then '1' else '0') str
      override' = map (\c -> if c == 'X' then '0' else c) str
   in Mask ( fromJust $ readBin mask') (fromJust $ readBin override')

parseSetMask :: GenParser Char st Operation
parseSetMask = do
  string "ask = "
  value <- many1 $ oneOf "01X"
  return $ SetMask $ parseMaskFromString value

parseWriteMem :: GenParser Char st Operation
parseWriteMem = do
  string "em["
  value1 <- many digit
  string "] = "
  value2 <- many digit
  return $ WriteMem (read value1) (read value2)

parseOperation :: GenParser Char st Operation
parseOperation = char 'm' >> (parseWriteMem <|> parseSetMask)

parseOperationFromString :: String -> Operation
parseOperationFromString line =
  either (error . show) id $
    parse parseOperation "cannot parse op" line

parseOperationsFromFile :: String -> IO [Operation]
parseOperationsFromFile fname = do
  content <- readFile fname
  return $ map parseOperationFromString $ lines content

printOps :: [Operation] -> IO ()
printOps [] = return ()
printOps ops = do
    print $ head ops
    printOps $ tail ops

applyMask :: Mask -> Int -> Int
applyMask (Mask mask' override') value =
    let maskedValue = mask' .&. value
    in maskedValue .|. override'

writeMem :: WriteMemOp
writeMem addr value = do
    (mem, currentMask) <- ask
    let newValue = applyMask currentMask value
        newMem = Map.insert addr newValue mem
    return (newMem, currentMask)

addressFloating :: Addr -> Int -> [Addr]
addressFloating addr m = do
  i <- [0..35]
  let shifted = 1 `shiftL` i
  if m .&. shifted > 0 then
    [addr .|. shifted]
  else []

powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = [x:ps | ps <- powerset xs] ++ powerset xs


addressPowerSet :: Addr -> Int -> [Addr]
addressPowerSet addr m =
  let addrs = addressFloating addr m
      sets = powerset addrs
  in map (foldl (.|.) addr) sets

writeMem2 :: WriteMemOp
writeMem2 addr newValue = do
    (mem, currentMask) <- ask
    let newAddr = (addr .|. override currentMask) `unset` mask currentMask
        floatingAddrs = addressPowerSet newAddr (mask currentMask)
        newMem = foldl (\memory floatingAddr ->
          Map.insert floatingAddr newValue memory) mem floatingAddrs
    return (newMem, currentMask)

runOps :: WriteMemOp -> [Operation] -> ReaderT MachineState Identity MachineState
runOps _ [] = do ask
runOps writeMemOp (op:ops) = do
    (newMem, newMask) <- case op of
        SetMask m -> do
            (mem, _) <- ask
            return (mem, m)
        WriteMem addr value -> do
            writeMemOp addr value
    local ( const (newMem, newMask) ) $ runOps writeMemOp ops


initialState :: MachineState
initialState = (Map.empty, Mask 0 0 )

unset :: Int -> Int -> Int
unset a b =
  let allSet = fromJust $ readBin (replicate 36 '1')
      mask = allSet `xor` b
  in a .&. mask

main :: IO ()
main = do
  ops <- parseOperationsFromFile "input_14a.txt"

  let finalState = runReader (runOps writeMem ops) initialState
      writtenValues = elems $ fst finalState
  print "part 1"
  print $ sum writtenValues

  let finalState = runReader (runOps writeMem2 ops) initialState
      writtenValues = elems $ fst finalState
  print "part 2"
  print $ sum writtenValues
