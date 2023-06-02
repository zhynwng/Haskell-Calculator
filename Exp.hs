-- | the expression and the evaluation

module Exp where 

import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Control.Applicative
-- Basic types
type Name       = String  -- Variable names are strings.
type Number     = Int     -- The kind of number in our language.
type Boolean    = Bool
type Function   = (Name, MathExp)
type Binds      = Map Name Result 

data MathExp
    = Number Number
    | Var    Name
    | Neg    MathExp
    | Plus   MathExp MathExp
    | Minus  MathExp MathExp
    | Mult   MathExp MathExp
    | Div    MathExp MathExp
    | Pow    MathExp MathExp
    -- Boolean Expression
    | Boolean Boolean
    | Not MathExp
    | Eq MathExp MathExp
    | Neq MathExp MathExp
    | Lt MathExp MathExp
    | LtEq MathExp MathExp
    | Gt MathExp MathExp
    | GtEq MathExp MathExp
    -- other Top-Level Expressions
    | Let [Name] [MathExp] MathExp
    | If MathExp MathExp MathExp
    | Lambda Function
    | App MathExp MathExp           -- we only do one variable lambda function here
    deriving (Eq, Show)

data Result = Result (Maybe Number) (Maybe Boolean) (Maybe (Binds,Function))

instance Show Result where
    show result = case result of
        Result (Just n) Nothing Nothing -> show n
        Result Nothing (Just b) Nothing -> show b
        Result Nothing Nothing (Just f) -> show f -- "error: can't show functions"
        otherwise                       -> "type error: can't match Int with Bool"


-- | "lifting" funtion for operators
evalNum f (Result n _ _) = Result (f <$> n) Nothing Nothing
evalNum2 f (Result n1 _ _) (Result n2 _ _) = Result (liftA2 f n1 n2) Nothing Nothing
evalBool f (Result _ b _ ) = Result Nothing (f <$> b) Nothing
evalBool2 f g (Result n1 b1 _) (Result n2 b2 _) = 
    case (b1, b2) of
        ((Just x), (Just y))    -> Result Nothing (liftA2 f b1 b2) Nothing
        otherwise               -> Result Nothing (liftA2 g n1 n2) Nothing

-- | This function might contain too much copy and paste....
evalMath :: Map Name Result -> MathExp -> Result
-- | evaluate calculation
evalMath _ (Number x)   = Result (Just x) Nothing Nothing
evalMath v (Neg x)      = (evalNum negate) (evalMath v x)
evalMath v (Plus x y)   = (evalNum2 (+)) (evalMath v x) (evalMath v y)
evalMath v (Minus x y)  = (evalNum2 (-)) (evalMath v x) (evalMath v y)
evalMath v (Mult x y)   =  (evalNum2 (*)) (evalMath v x) (evalMath v y)
evalMath v (Div x y)    =  (evalNum2 (div)) (evalMath v x) (evalMath v y)
evalMath v (Pow x y)    =  (evalNum2 (^)) (evalMath v x) (evalMath v y)

-- | evaluate boolean
evalMath _ (Boolean x)  = Result Nothing (Just x) Nothing
evalMath v (Not x)      = (evalBool not) (evalMath v x)
evalMath v (Eq x y)     = (evalBool2 (==) (==)) (evalMath v x) (evalMath v y) 
evalMath v (Neq x y)    = (evalBool2 (/=) (/=)) (evalMath v x) (evalMath v y) 
evalMath v (Lt x y)     = (evalBool2 (<) (<)) (evalMath v x) (evalMath v y) 
evalMath v (LtEq x y)   = (evalBool2 (<=) (<=)) (evalMath v x) (evalMath v y) 
evalMath v (Gt x y)     = (evalBool2 (>) (>)) (evalMath v x) (evalMath v y) 
evalMath v (GtEq x y)   = (evalBool2 (>=) (>=)) (evalMath v x) (evalMath v y) 

-- | evaluate let expression
evalMath v (Var x)      = case (Map.lookup x v) of
    (Just result)   -> result     
    Nothing         -> error "variable not in scope"
evalMath v (Let var varexp exp) = 
    let bind = Map.fromList $ zip var (map (evalMath v) varexp) in
        evalMath (Map.unionWith const bind v) exp       -- const here to shadow the variable

-- | evaluate if expression (type analysis are disregarded for now)
evalMath v (If x y z)   = 
    case show (evalMath v x) of
        "True"      -> (evalMath v y)
        "False"     -> (evalMath v z)
        otherwise   -> error "can't evaluate predicate" 

-- | evaluate lambda function
evalMath v (Lambda f)   = Result Nothing Nothing (Just (v,f))
evalMath v (App f x) = 
    case (evalMath v f) of
        Result Nothing Nothing (Just (binds, (name, exp)))  -> 
            let newBinds = Map.unionWith const binds v
                argument = Map.singleton name (evalMath v x)
            in
                evalMath (Map.unionWith const argument newBinds) exp 
        otherwise           -> error "can't apply an non-function argument"
        

eval :: MathExp -> Result
eval = evalMath Map.empty 
        