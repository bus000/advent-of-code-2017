{- You receive a signal directly from the CPU. Because of your recent assistance
 - with jump instructions, it would like you to compute the result of a series
 - of unusual register instructions.
 -
 - Each instruction consists of several parts: the register to modify, whether
 - to increase or decrease that register's value, the amount by which to
 - increase or decrease it, and a condition. If the condition fails, skip the
 - instruction without modifying the register. The registers all start at 0. The
 - instructions look like this:
 -
 - b inc 5 if a > 1
 - a inc 1 if b < 5
 - c dec -10 if a >= 1
 - c inc -20 if c == 10
 -
 - These instructions would be processed as follows:
 -
 -  * Because a starts at 0, it is not greater than 1, and so b is not modified.
 -  * a is increased by 1 (to 1) because b is less than 5 (it is 0).
 -  * c is decreased by -10 (to 10) because a is now greater than or equal to 1
 -    (it is 1).
 -  * c is increased by -20 (to -10) because c is equal to 10.
 -  * After this process, the largest value in any register is 1.
 -
 - You might also encounter <= (less than or equal to) or != (not equal to).
 - However, the CPU doesn't have the bandwidth to tell you what all the
 - registers are named, and leaves that to you to determine.
 -
 - What is the largest value in any register after completing the instructions
 - in your puzzle input? -}
module Main (main) where

import qualified Data.Map as Map
import qualified System.Exit as S
import qualified Text.Parsec as P
import qualified Text.Parsec.Number as P

type Var = String
type Amount = Int

data Cond
    = Lt Var Amount
    | Gt Var Amount
    | Eq Var Amount
    | Neq Var Amount
    | Leq Var Amount
    | Geq Var Amount
  deriving (Show)

data Stm
    = Dec Var Amount Cond
    | Inc Var Amount Cond
  deriving (Show)

type Program = [Stm]

type Registers = Map.Map Var Amount

main :: IO ()
main = do
    input <- parseProgram <$> getContents

    case input of
        Left err -> S.die (show err)
        Right program -> print $ maxMap (evaluate program Map.empty)

evaluate :: Program -> Registers -> Registers
evaluate program regs = foldl evaluateStm regs program

evaluateStm :: Registers -> Stm -> Registers
evaluateStm regs (Dec var amount cond)
    | evaluateCond cond regs = addToReg regs var (-amount)
    | otherwise = regs
evaluateStm regs (Inc var amount cond)
    | evaluateCond cond regs = addToReg regs var amount
    | otherwise = regs

evaluateCond :: Cond -> Registers -> Bool
evaluateCond (Lt var amount) regs = Map.findWithDefault 0 var regs < amount
evaluateCond (Gt var amount) regs = Map.findWithDefault 0 var regs > amount
evaluateCond (Eq var amount) regs = Map.findWithDefault 0 var regs == amount
evaluateCond (Neq var amount) regs = Map.findWithDefault 0 var regs /= amount
evaluateCond (Leq var amount) regs = Map.findWithDefault 0 var regs <= amount
evaluateCond (Geq var amount) regs = Map.findWithDefault 0 var regs >= amount

addToReg :: Registers -> Var -> Amount -> Registers
addToReg regs var amount = Map.insert var (previous + amount) regs
  where
    previous = Map.findWithDefault 0 var regs

parseProgram :: String -> Either P.ParseError Program
parseProgram = P.parse (statements <* P.eof) ""

type ProgramParser a = P.Parsec String () a

statements :: ProgramParser [Stm]
statements = P.endBy statement P.newline

statement :: ProgramParser Stm
statement = P.choice (map P.try [dec, inc])

dec :: ProgramParser Stm
dec = Dec <$> variable <* P.string " dec " <*> P.int <* P.string " if " <*>
    condition

inc :: ProgramParser Stm
inc = Inc <$> variable <* P.string " inc " <*> P.int <* P.string " if " <*>
    condition

variable :: ProgramParser Var
variable = P.many (P.oneOf ['a'..'z'])

condition :: ProgramParser Cond
condition = P.choice (map P.try [lt, gt, eq, neq, leq, geq])
  where
    lt = Lt <$> variable <* P.string " < " <*> P.int
    gt = Gt <$> variable <* P.string " > " <*> P.int
    eq = Eq <$> variable <* P.string " == " <*> P.int
    neq = Neq <$> variable <* P.string " != " <*> P.int
    leq = Leq <$> variable <* P.string " <= " <*> P.int
    geq = Geq <$> variable <* P.string " >= " <*> P.int

maxMap :: Ord v => Map.Map k v -> v
maxMap = maximum . Map.elems
