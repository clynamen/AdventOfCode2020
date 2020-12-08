import Text.ParserCombinators.Parsec
import Control.Monad.Trans.RWS.Lazy
    ( ask, get, put, runRWS, rws, RWS )
import Data.Set (Set)
import qualified Data.Set as Set

data VMState = VMState {
    ip :: Int,
    acc :: Int,
    lastOp :: Instruction,
    pastIp :: Set Int
} deriving (Eq, Show)

zeroVMState :: VMState
zeroVMState = VMState 0 0 Start Set.empty

data Instruction = Start | Nop Int | Acc Int | Jmp Int deriving (Show, Eq)

type Code = [Instruction]
type Output = [Int]

type VM = RWS Code Output VMState

newVM :: VM ()
newVM = rws (\_ s -> ((), s, []))

increaseIpBy :: Int -> VM ()
increaseIpBy n = do
    VMState currentIp acc lastOp pastIps <- get
    put $ VMState (currentIp + n) acc lastOp pastIps


runInstruction :: Instruction -> VM ()
runInstruction (Nop _) = increaseIpBy 1
runInstruction (Acc accValue) = do
    VMState currentIp acc _ pastIps <- get
    put $ VMState currentIp (acc+accValue) (Acc accValue) pastIps
    increaseIpBy 1
runInstruction (Jmp jmpValue) = do
    VMState currentIp acc _ pastIps <- get
    put $ VMState (currentIp+jmpValue) acc (Jmp jmpValue) pastIps

runNextInstruction :: VM ()
runNextInstruction = do
    instructions <- ask
    VMState currentIp _ _ pastIps <- get
    let nextInstruction = instructions !! currentIp
    if currentIp == length instructions
        then return ()
    else do
        runInstruction nextInstruction
        VMState currentIp' acc' _ _ <- get
        put $ VMState currentIp' acc' nextInstruction (Set.insert currentIp pastIps )


runWithInstructions :: [Instruction] -> VM Bool -> (Bool, VMState, Output)
runWithInstructions instructions vm =
    runRWS vm instructions zeroVMState


parseIntInstruction :: String -> (Int -> Instruction) -> GenParser Char st Instruction
parseIntInstruction name ctor =  do
    string name
    spaces
    optional $ char '+'
    value <- many $ noneOf " \n"
    return $ ctor (read value :: Int)

parseNop :: GenParser Char st Instruction
parseNop = parseIntInstruction "nop" Nop

parseAcc :: GenParser Char st Instruction
parseAcc = parseIntInstruction "acc" Acc

parseJmp :: GenParser Char st Instruction
parseJmp = parseIntInstruction "jmp" Jmp


parseInstructionLine :: GenParser Char st Instruction
parseInstructionLine = parseNop <|> parseAcc <|> parseJmp

parseInstruction :: String -> Instruction
parseInstruction line = either (error.show) id $
    parse parseInstructionLine "cannot parse statement" line

parseInstructions :: [String] -> [Instruction]
parseInstructions = map parseInstruction

parseInstructionsFromFile :: String -> IO [Instruction]
parseInstructionsFromFile fname = do
    content <- readFile fname
    return $ parseInstructions $ lines content

type StopCondition = VM Bool

isProgramStop :: StopCondition
isProgramStop = do
    VMState currentIp _ _ _ <- get
    instructions <- ask
    return $ currentIp == length instructions

instructionWasAlreadyExecuted :: StopCondition
instructionWasAlreadyExecuted = do
    VMState currentIp _ _ pastIps <- get
    return $ Set.member currentIp pastIps


runNInstructionsAtMost :: StopCondition -> Int -> VM Bool
runNInstructionsAtMost stopCond 0 = newVM >> stopCond
runNInstructionsAtMost stopCond 1 = runNextInstruction >> stopCond
runNInstructionsAtMost stopCond n = do
    stopped <- runNextInstruction >> stopCond
    if stopped then
        return True
    else
        runNInstructionsAtMost stopCond (n-1)

replaceAtIndex :: Int -> (a -> a) -> [a] -> [a]
replaceAtIndex i f xs = take i xs ++ [f (xs !! i)] ++ drop (i+1) xs

invertNopOrJmp :: Instruction -> Instruction
invertNopOrJmp (Nop x) = Jmp x
invertNopOrJmp (Jmp x) = Nop x

isNopOrJmp :: Instruction -> Bool
isNopOrJmp (Nop _) = True
isNopOrJmp (Jmp _) = True
isNopOrJmp _ = False

replaceNopOrJmpInstruction :: Int -> [Instruction] -> [Instruction]
replaceNopOrJmpInstruction i = replaceAtIndex i invertNopOrJmp

enumerate = zip [0..]

programsWithNopAndJmpReplaced :: [Instruction] -> [[Instruction]]
programsWithNopAndJmpReplaced instructions =
    [ replaceNopOrJmpInstruction i instructions | (i, inst) <- enumerate instructions , isNopOrJmp inst]


tripleFst (a, _, _) = a

firstResultWithStopWithInstructionReplacement :: [Instruction] -> (Bool, VMState, Output)
firstResultWithStopWithInstructionReplacement instructions =
    let programs = programsWithNopAndJmpReplaced instructions
        results = [runWithInstructions program $ runNInstructionsAtMost isProgramStop 1000 | program <- programs]
    in head $ [result | result <- results, tripleFst result]

main :: IO ()
main = do
    instructions <- parseInstructionsFromFile "input_08a.txt"
    print "Part one"
    print $ runWithInstructions instructions $ runNInstructionsAtMost instructionWasAlreadyExecuted 1000
    print "Part two"
    print $ firstResultWithStopWithInstructionReplacement instructions