{-# LANGUAGE FlexibleInstances, DeriveGeneric, DeriveAnyClass #-}

module Block4Unit1 where

import GHC.Generics
import FPPrac.Trees

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

-- ==== Self-explanation ====
-- sp = current free location in the stack

-- Type Aliases
type Stack  = [Int]
type Heap = [Int]
type Addr = Int

data Op     = Add | Mul | Sub
            deriving (Show, Generic, ToRoseTree)


data Instr  = PushConst Int
            | Calc Op

            -- Extended for Exercise 4-FP.3
            | PushAddr Addr -- Stack[sp] := Heap[addr]
            | Store Addr -- Heap[addr] := Stack[sp - 1]; sp := sp - 1

            -- Extended for Exercise 4-FP.7 
            | PushPC
            | EndRep

            | EndProg
            deriving (Show, Generic, ToRoseTree)


data Tick = Tick

data Expr = Const Int                   -- for constants
            | Var Addr                  -- for variables (extended for Exercise 4-FP.4)
            | BinExpr Op Expr Expr      -- for ``binary expressions''
            deriving (Show, Generic, ToRoseTree)


-- ========================================================================
-- Processor functions

xs <~ (i,a) = take i xs ++ [a] ++ drop (i+1) xs
                -- Put value a on position i in list xs

alu op = case op of
                Add -> (+)
                Mul -> (*)
                Sub -> (-)


core :: [Instr] -> (Int,Int,Heap,Stack) -> Tick -> (Int,Int,Heap,Stack)

core instrs (pc,sp,heap,stack) tick =  case instrs!!pc of

        PushConst n   -> (pc+1, sp+1 , heap , stack <~ (sp,n))

        Calc op  -> (pc+1, sp-1 , heap , stack <~ (sp-2,v))
                 where
                   v = alu op (stack!!(sp-2)) (stack!!(sp-1))

        -- Extended for Exercise 4-FP.3
        
        -- stack[sp] := heap[addr]
        PushAddr addr -> ( pc + 1, sp + 1, heap, stack <~(sp, v) )
            where
                v = heap!!addr

        -- heap[addr] := stack[sp - 1]
        -- sp := sp - 1 # free current location
        Store addr -> ( pc + 1, sp - 1, heap <~ (addr, v), stack)
            where
                v = stack!!(sp-1)

        -- Extended for Exercise 4-FP.7

        -- Push Current Program Counter
        PushPC -> ( pc + 1, sp + 1, heap, stack <~(sp, pc) )

        -- If numIter is non-zero
        EndRep -> if numIter > 0 
            -- Then pop pc' from the stack (as we will do PushPC again in next iteration),
            -- and then pc := pc'
            then ( pc', sp - 1, heap, stack <~ (sp-2, numIter) ) 
            -- Otherwise, pop 2 elements (pc, expr resp.) in the stack
            else ( pc + 1, sp - 2 , heap, stack )
            where
                pc' = stack!!(sp-1)
                numIter = stack!!(sp-2) - 1

        EndProg  -> (-1, sp, heap , stack)

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



-- Testing
clock      = repeat Tick
emptyStack = replicate 8 0
emptyHeap = replicate 16 0

test       = putStr
           $ unlines
           $ map show
           $ takeWhile (\(pc,_,_,_) -> pc /= -1)
           $ scanl (core prog) (0,0,emptyHeap,emptyStack) clock


-- Exercise 4-FP.5
class CodeGen a where
    codeGen :: a -> [Instr]

-- Exercise 4-FP.2
instance CodeGen Expr where
    codeGen (Const n) = [PushConst n]
    codeGen (Var addr) = [PushAddr addr]
    codeGen (BinExpr op e1 e2) = (codeGen e1) ++ (codeGen e2) ++ [Calc op]

-- Exercise 4-FP.4
data Stmnt = Assign Addr Expr
            | Repeat Expr [Stmnt]
    deriving (Show, Generic , ToRoseTree)

instance CodeGen Stmnt where
    codeGen (Assign addr expr) = codeGen expr ++ [Store addr]
    
    -- For Exercise 4-FP.7 
    -- Calcuate expr and store in the stack, Push pc into the stack, ...
    codeGen (Repeat expr stmnts) = codeGen expr ++ [PushPC] ++ codeGenForStmnts stmnts ++ [EndRep]
        where
            codeGenForStmnts :: [Stmnt] -> [Instr]
            codeGenForStmnts [] = []
            codeGenForStmnts (s:ss) = codeGen s ++ codeGenForStmnts ss

-- Exercise 4-FP.6
compile :: [Stmnt] -> [Instr]
compile [] = [EndProg]
compile (s:ss) = codeGen s ++ compile ss

-- Exercise 4-FP.8
codelines :: [Stmnt]
codelines = [
    Assign 0 (Const 0) -- sum := 0
    , Assign 1 (Const 1) -- j := 1
    , Repeat (Const 10) [ -- Repeat (10 times)
            Assign 0 (BinExpr Add (Var 0) (Var 1)) -- sum := sum + j
            , Assign 1 (BinExpr Add (Var 1) (Const 1)) -- j := j + 1
        ]
    ]

testmine = putStr
           $ unlines
           $ map show
           $ takeWhile (\(pc,_,_,_) -> pc /= -1)
           $ scanl (core $ compile codelines) (0,0,emptyHeap,emptyStack) clock