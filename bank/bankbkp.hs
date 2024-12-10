module Bank where

-- Define the BankOp type
newtype BankOp a = BankOp { runBankOpWith :: Float -> (a, Float) }

-- Helper function to execute BankOp with an initial balance of 0
runBankOp :: BankOp a -> a
runBankOp bankOp = fst (runBankOpWith bankOp 0.0)

-- Functor instance for BankOp
instance Functor BankOp where
    fmap f (BankOp g) = BankOp $ \balance ->
        let (result, newBalance) = g balance
        in (f result, newBalance)

-- Applicative instance for BankOp
instance Applicative BankOp where
    pure x = BankOp $ \balance -> (x, balance)
    (BankOp f) <*> (BankOp g) = BankOp $ \balance ->
        let (func, balance1) = f balance
            (result, balance2) = g balance1
        in (func result, balance2)

-- Monad instance for BankOp
instance Monad BankOp where
    return = pure
    (BankOp g) >>= f = BankOp $ \balance ->
        let (result, balance1) = g balance
            BankOp h = f result
        in h balance1

-- Deposit money into the account
deposit :: Float -> BankOp ()
deposit amount = BankOp $ \balance -> ((), balance + amount)

-- Withdraw money from the account with overdraft protection
withdraw :: Float -> BankOp Float
withdraw amount = BankOp $ \balance ->
    let maxOverdraft = -100
        newBalance = balance - amount
        actualWithdrawal = if newBalance < maxOverdraft
                           then balance - maxOverdraft
                           else amount
        finalBalance = max newBalance maxOverdraft
    in (actualWithdrawal, finalBalance)

-- Get the current balance
getBalance :: BankOp Float
getBalance = BankOp $ \balance -> (balance, balance)

-- Test operations to match expected output
testOperations :: IO ()
testOperations = do
    -- First sequence: deposit 50, withdraw 100, get balance
    let result1 = runBankOpWith (do
            deposit 50.0
            withdraw 100.0) 0.0
    print (fst result1)  -- Output: -50.0

    -- Second sequence: deposit 200, withdraw 300, get balance
    let result2 = runBankOpWith (do
            deposit 200.0
            withdraw 300.0
            getBalance) 0.0
    print result2  -- Output: (200.0, -100.0)

-- Main function for running the test
main :: IO ()
main = testOperations
