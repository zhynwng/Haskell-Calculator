-- | parse the input string into an expression

module Parser where

import Text.ParserCombinators.ReadP 
import Data.Char
import Data.List
import Data.Map.Strict
import Control.Monad
import Control.Exception
import Control.Applicative 
import Data.Function
import Exp

token :: a -> String -> ReadP a
token meaning name= skipSpaces *> string name *> pure meaning

parens :: ReadP a -> ReadP a
parens = between (skipSpaces *> char '(') (skipSpaces *> char ')')

skipString :: String -> ReadP ()
skipString s = skipSpaces *> string s *> pure ()

-- | parse a variable String
parseStrVar :: ReadP String
parseStrVar = skipSpaces *> ((:) <$> satisfy isLower <*> munch isAlphaNum)

-- | parse a pure math expression
parseCalc :: ReadP MathExp
parseCalc = prec0 where
    prec0 = chainl1 prec1 (token Plus "+" <++ token Minus "-")
    prec1 = prec2 <++ ((token Neg "-") <*> prec2)
    prec2 = chainl1 prec3 (token Mult "*" <++ token Div "/")
    prec3 = chainr1 prec4 (token Pow "^") 
    prec4 = chainl1 prec5 (string " " *> pure App)
    prec5 = parens parseBool <++ parseTopExp <++ parseVar <++ parseNum
    parseVar = Var <$> parseStrVar
    parseNum = skipSpaces *> (Number <$> read <$> munch1 isDigit)

-- | parse an Let expression  
parseLet :: ReadP MathExp
parseLet = pure Let
        <*> parseVars
        <*> parseBindExp
        <* skipString "in"
        <*> parseBool where
    parseVars = skipString "let" *> parseBindList parseStrVar
    parseBindExp = skipString "=" *> parseBindList parseBool
    parseBindList prec = (pure <$> prec) <++
        parens ((:) <$> prec <*> greedy (skipString "," *> prec))
greedy prec = ((:) <$> prec <*> greedy prec) <++ pure []

-- | parse boolean expressions (higher level than parseCalc)
parseBool :: ReadP MathExp
parseBool = prec0 where
    prec0 = ((token Not "not") <*> prec1) <++ prec1
    prec1 = chainBoolOps prec2 boolOps
    prec2 = parseLet <++ parseIf <++ parseCalc <++ parseLambda <++ parseSingleBool
    chainBoolOps p op = (pure (\x f y -> f x y) <*> p <*> op <*> p) +++ p
    boolOps = token Eq "==" <++ token Neq "/=" <++ token LtEq "<=" 
        <++ token GtEq ">=" <++ token Lt "<" <++ token Gt ">"
    parseSingleBool = Boolean <$>  
        (token True "True" <++ token False "False")
    
-- | parse if expression
parseIf :: ReadP MathExp
parseIf = pure If
    <* skipString "if"
    <*> parseBool
    <* skipString "then"
    <*> parseBool
    <* skipString "else"
    <*> parseBool

-- | parse lambda expression
parseLambda :: ReadP MathExp
parseLambda = parens $ fmap Lambda $ pure (,)
    <* skipString "\\"
    <*> parseStrVar
    <* skipString "->"
    <*> parseExp

parseTopExp :: ReadP MathExp
parseTopExp = parseLet <++ parseIf <++ parseLambda

parseExp :: ReadP MathExp
parseExp = parseLet <++ parseIf <++ parseBool

instance Read MathExp where
    readsPrec _ = readP_to_S parseExp
