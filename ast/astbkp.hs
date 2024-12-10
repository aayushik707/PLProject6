module Ast where

-- Define the data type for arithmetic expressions
data Expr
    = Literal Float           -- Represents a numeric literal
    | Plus Expr Expr          -- Addition
    | Minus Expr Expr         -- Subtraction
    | Times Expr Expr         -- Multiplication
    | Div Expr Expr           -- Division
    deriving (Show)           -- Enable printing of expressions for debugging

-- Evaluate an expression
eval :: Expr -> Float
eval (Literal x)       = x
eval (Plus left right) = eval left + eval right
eval (Minus left right) = eval left - eval right
eval (Times left right) = eval left * eval right
eval (Div left right)   = eval left / eval right

-- Test expressions and their evaluations
test1 :: Float
test1 = eval $ Plus (Literal 3.0) (Literal 2.0)  -- Expected: 5.0

test2 :: Float
test2 = eval $ Plus (Literal 3.0) (Div (Literal 1.0) (Literal 2.0))  -- Expected: 3.5

test3 :: Float
test3 = eval $ Plus (Times (Literal 3.0) (Literal 5.0)) (Div (Literal 1.0) (Literal 2.0))  -- Expected: 15.5

-- Main function to output results
main :: IO ()
main = do
    print test1  -- Output: 5.0
    print test2  -- Output: 3.5
    print test3  -- Output: 15.5
