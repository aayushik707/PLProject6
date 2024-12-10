module Bank where

-- Define the BankOp type using a stateful monadic approach
newtype BankOp a = BankOp { runBankOpWith :: Float -> (a, Float) }

-- Helper function to execute a BankOp and extract the result
runBankOp :: BankOp a -> a
runBankOp (BankOp op) = fst (op 0.0)

-- Deposit operation: add the given amount to the balance
deposit :: Float -> BankOp ()
deposit amount = BankOp $ \balance -> ((), balance + amount)

-- Withdraw operation: deduct the given amount while enforcing overdraft limit
withdraw :: Float -> BankOp Float
withdraw amount = BankOp $ \balance ->
    let overdraftLimit = -100
        availableWithdrawal = balance - overdraftLimit
        actualWithdrawal = if amount > availableWithdrawal then availableWithdrawal else amount
        newBalance = balance - actualWithdrawal
    in (actualWithdrawal, newBalance)

-- Get the current balance
getBalance :: BankOp Float
getBalance = BankOp $ \balance -> (balance, balance)

-- Monad instance for BankOp
instance Monad BankOp where
    (BankOp op) >>= f = BankOp $ \balance ->
        let (result, newBalance) = op balance
            BankOp nextOp = f result
        in nextOp newBalance

-- Functor instance for BankOp
instance Functor BankOp where
    fmap func (BankOp op) = BankOp $ \balance ->
        let (result, newBalance) = op balance
        in (func result, newBalance)

-- Applicative instance for BankOp
instance Applicative BankOp where
    pure x = BankOp $ \balance -> (x, balance)
    (BankOp fOp) <*> (BankOp xOp) = BankOp $ \balance ->
        let (func, balance1) = fOp balance
            (x, balance2) = xOp balance1
        in (func x, balance2)
