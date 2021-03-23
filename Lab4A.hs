-- Authors:
-- Date:

import Poly
import Test.QuickCheck


-- Use the following simple data type for binary operators
data BinOp = AddOp | MulOp

--------------------------------------------------------------------------------
-- * A1
-- Define a data type Expr which represents three kinds of expression:
-- binary operators (use BinOp as a helper type) applied to two expressions,
-- numbers (use Int), and exponentiation x^n.
-- Note that since we consider expressions containing just a single variable,
-- x, your data type should not use String or Char anywhere, since this is
-- not needed.

data Expr = Con Int | Op BinOp Expr Expr | Pow Int


--------------------------------------------------------------------------------
-- * A2
-- Define the data type invariant that checks that exponents are never negative
prop_Expr :: Expr -> Bool
prop_Expr (Con _)      = True
prop_Expr (Op _ e1 e2) = (prop_Expr e1) && (prop_Expr e2)
prop_Expr (Pow x)      = 0 <= x




--------------------------------------------------------------------------------
-- * A3
-- Make Expr an instance of Show (along the lines of the example in the lecture)
-- You can use Haskell notation for powers: x^2
-- You should show x^1 as just x. 


showExpr :: Expr -> String
showExpr e = case e of
  Con n               -> show n
  Op AddOp e1 e2      -> showExpr e1 ++ " + " ++ showExpr e2
  Op MulOp (Con 1) e  -> showExpr e
  Op MulOp e (Con 1)  -> showExpr e
  Op MulOp e1 e2      -> showExpr e1 ++ " * " ++ showExpr e2
  Pow 1               ->  "x"
  Pow 0               ->  "1"
  Pow n               ->  "x^" ++ show n


instance Show Expr where
  show = showExpr
--------------------------------------------------------------------------------
-- * A4
-- Make Expr and instance of Arbitrary.
-- Now you can check the data type invariant that you defined in A3 using
-- quickCheck

-- (Optional)
-- Add a definition of function shrink :: Expr -> [Expr] to Arbitrary
-- which gives hints to quickCheck on possible smaller expressions that it
-- could use to find a smaller counterexample for failing tests

genExpr :: Int -> Gen Expr
genExpr n = frequency [(1, genCon), (1, genPow), (n, genOp)]
 where
  genCon = do
    n <- choose (0, 10)
    return (Con n)

  genPow = do
    n <- choose (0, 10)
    return (Pow n)

  genOp = let m = n `div` 2 in do
    op <- elements [AddOp, MulOp]
    e1 <- genExpr m
    e2 <- genExpr m
    return (Op op e1 e2)


instance Arbitrary Expr where
    arbitrary = sized genExpr 


--------------------------------------------------------------------------------
-- * A5
-- Define the eval function which takes a value for x and an expression and
-- evaluates it

eval :: Int -> Expr -> Int
eval x e = case e of
  Pow 0          -> 1
  Pow 1          -> x
  Pow n          -> x* (eval x (Pow (n-1)))
  Con n          -> n
  Op AddOp e1 e2 -> (eval x e1) + (eval x e2)
  Op MulOp e1 e2 -> (eval x e1) * (eval x e2) 
  
--------------------------------------------------------------------------------
-- * A6
exprToPoly :: Expr -> Poly
exprToPoly (Con n)          = fromList [n]
exprToPoly (Pow n)          = fromList (1 : (replicate n 0))
exprToPoly (Op AddOp e1 e2) = exprToPoly e1 + exprToPoly e2
exprToPoly (Op MulOp e1 e2) = exprToPoly e1 * exprToPoly e2

-- Define
-- Which converts an expression into a polynomial.
-- Here it is important to think recursively to just solve the bigger problem
-- by solving the smaller problems and combining them in the right way. 

-- Define (and check) prop_exprToPoly, which checks that evaluating the
-- polynomial you get from exprToPoly gives the same answer as evaluating
-- the expression

prop_exprToPoly x e = eval x e == evalPoly x (exprToPoly e)

--------------------------------------------------------------------------------
-- * A7
-- Now define the function going in the other direction, 
polyToExpr :: Poly -> Expr
polyToExpr p | toList p == []         = Con 0
polyToExpr p 
  | xs ==  []                         = Con x
  | x >  1 && xs == filter (== 0) xs  = mul (Con x) (Pow (length xs))
  | x == 1 && xs == filter (== 0) xs  = (Pow (length xs))
  | x == 1 && xs /= filter (== 0) xs  = add (Pow (length xs)) (polyToExpr (fromList xs))
  | x == 0                            = polyToExpr $ fromList xs
  | otherwise                         = add (mul (Con x) (Pow (length xs))) (polyToExpr (fromList xs))
  where
    x:xs = toList p
    add = Op AddOp
    mul = Op MulOp



-- Write (and check) a quickCheck property for this function similar to
-- question 6. 
prop_polyToExpr x p = evalPoly x p == eval x (polyToExpr p)
--------------------------------------------------------------------------------
-- * A8
-- Write a function
simplify :: Expr -> Expr
-- which simplifies an expression by converting it to a polynomial
-- and back again
simplify e = polyToExpr ( exprToPoly e)

--------------------------------------------------------------------------------
-- * A9
-- Write a quickCheck property
prop_noJunk :: Expr -> Bool

--that checks that a simplified expression does not contain any "junk":
--where junk is defined to be multiplication by one or zero,
--addition of zero, addition or multiplication of numbers, or x to the
--power zero. (You may need to fix A7)

prop_noJunk e = case e' of
   Op MulOp (Con 1) _ -> False
   Op MulOp _ (Con 1) -> False
   Op MulOp (Con 0) _ -> False
   Op MulOp _ (Con 0) -> False
   Op AddOp (Con 0) _ -> False
   Op AddOp _ (Con 0) -> False
   Con _              -> True
   Pow 0              -> False
   Pow _              -> True
   Op _ e1 e2         -> (prop_noJunk e1) && (prop_noJunk e2)
  where e' = simplify e
--------------------------------------------------------------------------------