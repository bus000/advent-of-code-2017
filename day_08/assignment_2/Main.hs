{- To be safe, the CPU also needs to know the highest value held in any register
 - during this process so that it can decide how much memory to allocate to
 - these operations. For example, in the above instructions, the highest value
 - ever held was 10 (in register c after the third instruction was
 - evaluated). -}
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
        Right program ->
            let states = evaluate program Map.empty
                nonEmpty = filter (not . Map.null) states
            in print $ maximum (map maxMap nonEmpty)

evaluate :: Program -> Registers -> [Registers]
evaluate program regs = scanl evaluateStm regs program

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
