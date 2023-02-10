{-# LANGUAGE LambdaCase #-}

module Calculus where

-- Math expressions.
data Expr
  = X              --  x
  | Const Int      --  22
  | Add Expr Expr  --  a + b
  | Mul Expr Expr  --  a * b
  | Pow Expr Expr  --  a ^^^ b
  | Neg Expr       --  -a
  | Der Expr       --  d/dx (a)
  deriving Eq

-- Syntax helpers.

-- Get +, -, * from the Num type class.
instance Num Expr where
  (+) = Add
  a - b = a + Neg b
  (*) = Mul
  abs = undefined
  signum = undefined
  fromInteger a = Const (fromInteger a)
  negate = Neg

-- X variable reference.
x :: Expr
x = X

-- Power operator.
infixr 8 ^^^
(^^^) :: Expr -> Expr -> Expr
(^^^) = Pow

-- Derivative "operator".
d'dx :: Expr -> Expr
d'dx = Der

-- Pretty print math expressions.
instance Show Expr where
  show = \case
    X -> "x"
    Const a -> show a
    Add a b -> "(" <> show a <> " + " <> show b <> ")" 
    Mul a b -> "(" <> show a <> " * " <> show b <> ")" 
    Pow a b -> "(" <> show a <> " ^^^ " <> show b <> ")" 
    Neg a -> "(- " <> show a <> ")" 
    Der a -> "(d'dx " <> show a <> ")"

-- Solve derivatives.
solve :: Expr -> Expr
solve = \case

  -- Single variable rule.
  Der X -> 1

  -- Constant rule.
  Der (Const _) -> 0

  -- Sum rule.
  Der (Add a b) -> d'dx' a + d'dx' b

  -- Constant multiple rule.
  Der (Mul (Const a) b) -> Const a * d'dx' b
  Der (Mul a (Const b)) -> d'dx' a * Const b
  Der (Neg a) -> - d'dx' a

  -- Power rule.
  Der (Pow X (Const b)) -> Const b * x ^^^ (Const b - 1)

  -- Give up.
  a -> a

  where

  d'dx' a = solve (d'dx a)

-- Simplify expressions.  Basic bottom-up reductions.
simplify :: Expr -> Expr
simplify = \case
  Neg a -> case simplify a of
    Const a -> Const (- a)
  Add a b -> case (simplify a, simplify b) of
    (Const a, Const b) -> Const (a + b)
    (a, Const 0) -> a
    (Const 0, b) -> b
    (a, b) -> Add a b
  Mul a b -> case (simplify a, simplify b) of
    (Const a, Const b) -> Const (a * b)
    (_, Const 0) -> Const 0
    (Const 0, _) -> Const 0
    (a, Const 1) -> a
    (Const 1, b) -> b
    (a, b) -> Mul a b
  Pow a b -> case (simplify a, simplify b) of
    (_, Const 0) -> Const 1
    (a, Const 1) -> a
    (a, b) -> Pow a b
  Der a -> d'dx (simplify a)
  a -> a

-- Simple example.
example :: Expr
example = d'dx (3 * x ^^^ 2 + 10 * x + 8)

-- Run it!
main :: IO ()
main = do
  putStrLn ""
  putStrLn "Hey Kimo!  You proud of me?  I'm the first dude here!  What's calculus?"
  putStrLn ""
  putStrLn $ "Expression:  " <> show example
  putStrLn $ "Solved:      " <> show (solve example)
  putStrLn $ "Simplified:  " <> show (simplify (solve example))
  putStrLn ""


