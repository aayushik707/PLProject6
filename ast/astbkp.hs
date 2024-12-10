module Ast where


data Expr
    = Literal Float           -- Represents a literal value
    | Plus Expr Expr          -- Addition
    | Minus Expr Expr         -- Subtraction
    | Times Expr Expr         -- Multiplication
    | Div Expr Expr           -- Division
    deriving (Show)

-- Evaluate the expression
eval :: Expr -> Float
eval (Literal x)     = x
eval (Plus e1 e2)    = eval e1 + eval e2
eval (Minus e1 e2)   = eval e1 - eval e2
eval (Times e1 e2)   = eval e1 * eval e2
eval (Div e1 e2)     = eval e1 / eval e2


test1 :: Float
test1 = eval $ Plus (Literal 3.0) (Literal 2.0)

test2 :: Float
test2 = eval $ Plus (Literal 3.0) (Div (Literal 1.0) (Literal 2.0))

test3 :: Float
test3 = eval $ Plus (Times (Literal 3.0) (Literal 5.0)) (Div (Literal 1.0) (Literal 2.0))

-- Main function to output the results of the tests
main :: IO ()
main = do
    print test1  -- Expected output: 5.0
    print test2  -- Expected output: 3.5
    print test3  -- Expected output: 15.5
