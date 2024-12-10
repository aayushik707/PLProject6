module Ast where

-- Define the Expr data type
data Expr = Plus Expr Expr | Minus Expr Expr | Times Expr Expr | Div Expr Expr
    | Literal Float
    deriving (Show) -- Added Show for better debugging

-- Evaluate the expression
eval :: Expr -> Float
eval (Literal x)       = x
eval (Plus expr1 expr2)  = eval expr1 + eval expr2
eval (Minus expr1 expr2) = eval expr1 - eval expr2
eval (Times expr1 expr2) = eval expr1 * eval expr2
eval (Div expr1 expr2)   = eval expr1 / eval expr2

-- Test cases as expressions
test1 :: Expr
test1 = Plus (Literal 3.0) (Literal 2.0) -- Should evaluate to 5.0

test2 :: Expr
test2 = Plus (Literal 3.0) (Div (Literal 1.0) (Literal 2.0)) -- Should evaluate to 3.5

test3 :: Expr
test3 = Plus (Times (Literal 3.0) (Literal 5.0)) (Div (Literal 1.0) (Literal 2.0)) -- Should evaluate to 15.5
