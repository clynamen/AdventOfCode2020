data Policy = Policy {
    fstNum :: Int,
    sndNum :: Int,
    policyChar :: Char
} deriving Show

type Password = String

lineToPolicyAndPassword :: String -> (Policy, Password)
lineToPolicyAndPassword line =
    let (policyString, pw_) = break (==':') line
        pw = drop 2 pw_
        (minMaxString, ch_) = break (==' ') policyString
        ch = drop 1 ch_
        (minStr, maxStr_) = break (=='-') minMaxString
        maxStr = drop 1 maxStr_
    in (Policy (read minStr) (read maxStr) (head ch), pw)

getInputPolicyAndPasswords :: String -> IO [(Policy, Password)]
getInputPolicyAndPasswords fname = do
    fileContent <- readFile fname
    let lines_ = lines fileContent
    return $ map lineToPolicyAndPassword lines_

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0

countCharIn :: Char -> String -> Int
countCharIn query str = sum $ map (boolToInt . (==query)) str

type Rule = Policy -> Password -> Bool

isPasswordValidRule1 :: Rule
isPasswordValidRule1 policy pw =
    let count = countCharIn (policyChar policy) pw
    in  fstNum policy <= count && count <= sndNum policy

xor p q = (p || q) && not (p && q)
infixr 3 `xor`

isPasswordValidRule2 :: Rule
isPasswordValidRule2 policy pw =
    let fstChar = pw !! (fstNum policy-1)
        sndChar = pw !! (sndNum policy-1)
        query = policyChar policy
    in  query == fstChar `xor` query == sndChar

countValidPasswords :: Rule -> [(Policy, Password)] -> Int
countValidPasswords rule policyAndPasswords =
    sum $ map (boolToInt . uncurry rule) policyAndPasswords

main = do
    policyAndPasswords <- getInputPolicyAndPasswords "input_02a.txt"
    print $ countValidPasswords isPasswordValidRule1 policyAndPasswords
    print $ countValidPasswords isPasswordValidRule2 policyAndPasswords