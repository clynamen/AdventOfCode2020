import Control.Monad.Trans.RWS.Lazy
import Data.Functor.Identity

newtype VMState = VMState {
    ip :: Int
} deriving (Eq, Show)

zeroVMState = VMState 0

data Instruction = Nop | Acc Int | Jmp Int

type Code = [Instruction]
type Output = [Int]

type VM = RWS Code Output VMState

newVM :: VM ()
newVM = rws (\r -> \s -> ((), s, []))

runNextInstruction :: VM ()
runNextInstruction = do
    instructions <- ask
    currentVMState <- get
    let currentIp = ip currentVMState
        nextInstruction = instructions !! currentIp
        newVMState = VMState (currentIp+1)
    put newVMState

instructions :: [Instruction]
instructions = [Acc 1, Acc 2, Acc 3]


-- runNInstructions' :: VMState -> [Instruction] -> Int -> ((), VMState, Output)
-- runNInstructions' currentVMState instructions 0 = ((), currentVMState, [])
-- runNInstructions' currentVMState instructions n =
--     let (_, newState, output) = runRWS runNextInstruction instructions currentVMState
--     in runNInstructions' newState instructions (n-1)

-- runNInstructions :: [Instruction] -> Int -> ((), VMState, Output)
-- runNInstructions = runNInstructions' zeroVMState

-- runNInstructions' :: VMState -> [Instruction] -> Int -> ((), VMState, Output)
-- runNInstructions' currentVMState instructions 0 = ((), currentVMState, [])
-- runNInstructions' currentVMState instructions n =
--     let (_, newState, output) = runRWS runNextInstruction instructions currentVMState
--     in runNInstructions' newState instructions (n-1)

runWithInstructions :: [Instruction] -> VM () -> ((), VMState, Output)
runWithInstructions instructions vm = 
    runRWS vm instructions zeroVMState

runNInstructions :: Int -> VM ()
runNInstructions 1 = runNextInstruction
runNInstructions n = do
    runNextInstruction
    runNInstructions (n-1)

main :: IO ()
main = do
    print $ runWithInstructions instructions $ runNInstructions 1
    print $ runWithInstructions instructions $ runNInstructions 2
    print $ runWithInstructions instructions $ runNInstructions 3
    print $ runWithInstructions instructions $ runNInstructions 4