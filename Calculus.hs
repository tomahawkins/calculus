{-# LANGUAGE LambdaCase #-}

module Calculus where

-- Math expressions.
data Expr
  = X              --  Variable
  | Const Int      --  Constant
  | Add Expr Expr  --  a + b
  | Mul Expr Expr  --  a * b
  | Pow Expr Expr  --  a ^^^ b
  | Neg Expr       --  -a
  | Der Expr       --  d/dx (a)
  deriving Eq

-- Get +, -, * syntax for free.
instance Num Expr where
  (+) = Add
  a - b = a + Neg b
  (*) = Mul
  abs = undefined
  signum = undefined
  fromInteger a = Const (fromInteger a)
  negate = Neg

-- Syntax helpers.

-- X variable reference.
x :: Expr
x = X

-- Derivative "operator".
d'dx :: Expr -> Expr
d'dx = Der

-- Power operator.
infixr 8 ^^^
(^^^) :: Expr -> Expr -> Expr
(^^^) = Pow

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

-- Differentiate.  Symbolically compute the derivative.
diff :: Expr -> Expr
diff = \case

  -- Single variable rule.
  Der X -> 1

  -- Constant rule.
  Der (Const _) -> 0

  -- Sum rule.
  Der (Add a b) -> diff (d'dx a) + diff (d'dx b)

  -- Constant multiple rule.
  Der (Mul (Const a) b) -> Const a * diff (d'dx b)
  Der (Mul a (Const b)) -> diff (d'dx a) * Const b
  Der (Neg a) -> - diff (d'dx a)

  -- Power rule.
  Der (Pow X (Const b)) -> Const b * x ^^^ (Const b - 1)

  -- Give up.
  a -> a

-- Simplify expressions.  Basic bottom-up reductions.
simplify :: Expr -> Expr
simplify = \case
  Neg (Const a) -> Const (- a)
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
example = d'dx (x ^^^ 2 + 10 * x + 3)

-- Run it!
main :: IO ()
main = do
  putStrLn "Hey Kimo!  You proud of me?  I'm the first dude here!  What's calculus?"
  putStrLn ""
  putStrLn $ "Original problem:    " <> show example
  putStrLn $ "Differentiation:     " <> show (diff example)
  putStrLn $ "With simplification: " <> show (simplify (diff example))
  putStrLn ""


