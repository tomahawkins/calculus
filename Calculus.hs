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
  Der (Mul a (Const b)) -> Const b * d'dx' a
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

  -- Negation of constants.
  Neg a -> case simplify a of
    Const a -> Const (- a)
    a -> Neg a

  -- Addition reductions.  If there is a constant, always return it on the right branch.
  Add a b -> case (simplify a, simplify b) of
    (Const a, Const b) -> Const (a + b)
    (a, Const 0) -> a
    (Const 0, b) -> b
    (Const a, Add b0 (Const b1)) -> Add b0 (Const (a + b1))
    (Add a0 (Const a1), Const b) -> Add a0 (Const (a1 + b))
    (Add a0 (Const a1), Add b0 (Const b1)) -> Add (Add a0 b0) (Const (a1 + b1))
    (Add a0 (Const a1), b) -> Add (Add a0 b) (Const a1)
    (a, Add b0 (Const b1)) -> Add (Add a b0) (Const b1)
    (Const a, b) -> Add b (Const a)
    (a, Const b) -> Add a (Const b)
    (a, b) -> Add a b

  -- Multiply reductions.  If there is a constant, always return it on the left branch.
  Mul a b -> case (simplify a, simplify b) of
    (Const a, Const b) -> Const (a * b)
    (_, Const 0) -> Const 0
    (Const 0, _) -> Const 0
    (a, Const 1) -> a
    (Const 1, b) -> b
    (Const a, Mul (Const b0) b1) -> Mul (Const (a * b0)) b1
    (Mul (Const a0) a1, Const b) -> Mul (Const (a0 * b)) a1
    (Mul (Const a0) a1, Mul (Const b0) b1) -> Mul (Const (a0 * b0)) (Mul a1 b1)
    (Mul (Const a0) a1, b) -> Mul (Const a0) (Mul a1 b)
    (a, Mul (Const b0) b1) -> Mul (Const b0) (Mul a b1)
    (Const a, b) -> Mul (Const a) b
    (a, Const b) -> Mul (Const b) a
    (a, b) -> Mul a b

  -- Power of 0 and power of 1 simplifications.
  Pow a b -> case (simplify a, simplify b) of
    (_, Const 0) -> Const 1
    (a, Const 1) -> a
    (a, b) -> Pow a b

  -- Decensd below Der to simplify subexpression.
  Der a -> d'dx (simplify a)

  X -> x
  Const a -> Const a

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


