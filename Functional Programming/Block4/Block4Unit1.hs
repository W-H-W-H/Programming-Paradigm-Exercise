{-# LANGUAGE FlexibleInstances, DeriveGeneric, DeriveAnyClass #-}

module Block4Unit1 where

import GHC.Generics
import FPPrac.Trees

{-
    The exercise isn't completed.
-}

{-
Extension of CoreIntro.hs:
- instructions as program *in* the processor,
- stack now is list of fixed length,i
- clock added
- program counter + stack pointer added,
- instruction EndProg added,
- update operaton (<~) added,
-}

-- ========================================================================

type Stack  = [Int]
type Heap = [Int]

data Op     = Add | Mul | Sub
            deriving (Show, Generic, ToRoseTree)


data Instr  = PushConst Int
            | Calc Op
            | EndProg

            -- Extended for Exercise 4-FP.3
            | Store Int
            | PushAddr Int

            -- Extended for Exercise 4-FP.7
            | PushPC
            | EndRep
            deriving (Show, Generic, ToRoseTree)


data Tick = Tick

data Expr = Const Int                   -- for constants
          | BinExpr Op Expr Expr        -- for ``binary expressions''

          -- Extended for Exercise 4-FP.4 (Guess)
          | Var Int                     -- for variables (1st param is address in Heap)

            deriving (Show, Generic, ToRoseTree)

-- ========================================================================
-- Processor functions

xs <~ (i,a) = take i xs ++ [a] ++ drop (i+1) xs
                -- Put value a on position i in list xs

alu op = case op of
                Add -> (+)
                Mul -> (*)
                Sub -> (-)


core :: [Instr] -> (Int, Int, Stack, Heap) -> Tick -> (Int, Int, Stack, Heap)

core instrs (pc, sp, stack, heap) tick =  case instrs!!pc of

        PushConst n   -> (pc+1, sp+1 , stack <~ (sp,n) , heap )

        Calc op  -> (pc+1, sp-1 , stack <~ (sp-2,v) , heap )
                 where
                   v = alu op (stack!!(sp-2)) (stack!!(sp-1))

        EndProg  -> (-1, sp, stack, heap)

        -- Extended for Exercise 4-FP.3

        PushAddr addr -> (pc+1, sp+1, stack <~(sp, v), heap)
            where
                v = heap!!addr

        Store addr -> (pc + 1, sp-1, stack, heap <~ (addr, stack!!(sp-1) ) )

        -- Extended for Exercise 4-FP.7
        PushPC -> (pc + 1, sp + 1, stack <~ (sp, pc), heap)



-- ========================================================================
-- example Program for expression: (((2*10) + (3*(4+11))) * (12+5))

-- Tree of this expression of type Expr (result of parsing):
expr = BinExpr Mul
          (BinExpr Add
              (BinExpr Mul
                  (Const 2)
                  (Const 10))
              (BinExpr Mul
                  (Const 3)
                  (BinExpr Add
                      (Const 4)
                      (Const 11))))
          (BinExpr Add
              (Const 12)
              (Const 5))

expr2 = BinExpr Add 
    (Const 1) 
    (BinExpr 
        Mul (Const 3) (Const 4)
    )


-- The program that results in the value of the expression (1105):
prog = [ PushConst 2
       , PushConst 10
       , Calc Mul
       , PushConst 3
       , PushConst 4
       , PushConst 11
       , Calc Add
       , Calc Mul
       , Calc Add
       , PushConst 12
       , PushConst 5
       , Calc Add
       , Calc Mul
       , EndProg
       ]

prog2 = [ PushConst 2 , PushConst 10, Calc Mul, Store 3 , PushAddr 2 , EndProg]

-- Testing
clock      = repeat Tick
emptyStack = replicate 8 0
emptyHeap = replicate 16 0

test       = putStr
           $ unlines
           $ map show
           $ takeWhile (\(pc,_,_,_) -> pc /= -1)
           $ scanl (core prog) (0,0,emptyStack,emptyHeap) clock

-- My Testing function
testProg prog = putStr
    $ unlines $ map show $ takeWhile (\(pc,_,_,_) -> pc /= -1)
    $ scanl (core prog) (0,0,emptyStack,emptyHeap) clock


type Prog = [Instr]

-- Exercise 4-FP.1

-- (1)
-- $> test
-- $> toRoseTree expr

-- Exercise 4-FP.2
codeGen :: Expr -> [Instr]
codeGen e = codeGenHelper e ++ [EndProg]
    where
        codeGenHelper :: Expr -> [Instr]
        codeGenHelper (Const n) = PushConst n : []
        codeGenHelper (Var addr) = PushAddr addr : []
        codeGenHelper (BinExpr op e1 e2) = codeGenHelper e1 ++ codeGenHelper e2 ++ (Calc op : [])

-- Exercise 4-FP.3 (Seems OK)

-- Exercise 4-FP.4 (Seems OK)
data Stmnt = Assign Int Expr 
    | Repeat Expr [Stmnt]
    deriving (Show, Generic , ToRoseTree)

codeGen' ::Stmnt -> [Instr]
codeGen' (Assign addr expr) = init (codeGen expr) ++ (Store addr : EndProg : [] )


-- Exercise 4-FP.5
class CodeGen a where
    codeGen'' :: a -> [Instr]

instance CodeGen Expr where
    codeGen'' = codeGen

instance CodeGen Stmnt where
    codeGen'' = codeGen'

-- Exercise 4-FP.6
compile :: [Stmnt] -> [Instr]
compile [] = EndProg : []
compile (s:stmnts) =  init (codeGen'' s) ++ compile stmnts

-- Exercise 4-FP.7
