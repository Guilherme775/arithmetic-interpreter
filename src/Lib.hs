{-# LANGUAGE LambdaCase #-}

module Lib ( eval ) where
import Text.Parsec
import Text.Parsec.Error (newErrorMessage, Message (Message))
import Text.Parsec.Pos (initialPos)

data Instruction = Add | Sub | Mult | Div | Number Double deriving (Show, Eq, Ord)

numberParser :: Parsec String st Instruction
numberParser = Number <$> (read <$> many1 digit)

operatorParser :: Parsec String st Instruction
operatorParser = chooseOp <$> oneOf "+-*/"
                   where chooseOp '+' = Add
                         chooseOp '-' = Sub
                         chooseOp '*' = Mult
                         chooseOp '/' = Div
                         chooseOp _ = undefined

expressionParser :: Parsec String st [Instruction]
expressionParser = between (char '(') (char ')') elements
                    where elements = (try numberParser <|> try operatorParser) `sepBy1` (char ' ')

interpreter :: [Instruction] -> Either ParseError Double
interpreter = \case
    [Number n] -> Right n
    ((Number x) : Add : (Number y) : xs) -> interpreter (Number (x + y) : xs)
    ((Number x) : Sub : (Number y) : xs) -> interpreter (Number (x - y) : xs)
    ((Number x) : Mult : (Number y) : xs) -> interpreter (Number (x * y) : xs)
    ((Number x) : Div : (Number y) : xs) -> interpreter (Number (x / y) : xs)
    x -> Left (newErrorMessage (Message (show x)) (initialPos "Invalid expression"))

eval :: String -> Either ParseError Double
eval input = parse expressionParser "unknown" input >>= interpreter
