module Potential.Printing where

import Numeric (showHex)

import Potential.MachineState( Reg(..) )
import Potential.Assembly

instance Show Reg where
  show Rax = "%rax"
  show Rbx = "%rbx"
  show Rcx = "%rcx"
  show Rdx = "%rdx"
  show Rsi = "%rsi"
  show Rdi = "%rdi"
  show Rbp = "%rbp"
  show Rsp = "%rsp"
  show Rflags = "%rflags"
  show Rip = "%rip"
  show R08 = "%r08"
  show R09 = "%r09"
  show R10 = "%r10"
  show R11 = "%r11"
  show R12 = "%r12"
  show R13 = "%r13"
  show R14 = "%r14"
  show R15 = "%r15"


instance Show Deref where
  show (Deref mem_location (base, index, scale)) =
    show mem_location ++ show (base, index, scale)
  show (Deref2 mem_location base) =
    show mem_location ++ "(" ++ show base ++ ")"
  show (Deref3 base) =
    "(" ++ show base ++ ")"

instance Show Instr where
  show (Cmt s)     = "// " ++ s
  show (Ld s d)    = "mov " ++ show s ++ " " ++ show d
  show (Sto s d)   = "mov " ++ show s ++ " " ++ show d
  show (Mov r1 r2) = "mov " ++ show r1 ++ " " ++ show r2
  show (Push r)    = "push " ++ show r
  show (Pop r)     = "pop " ++ show r
  show (Cmp r1 r2) = "cmp " ++ show r1 ++ " " ++ show r2
  show (Je r)      = "je " ++ show r
  show (Jne r)     = "jne " ++ show r
  show (Jmp r)     = "jmp " ++ show r
  show (SJmp f)    = "jmp " ++ fnname f -- ++ " (defined at " ++ filename f ++ ":" ++ show (fileline f) ++ ")"
  show (Call r)    = "call " ++ show r
  show (SCall f)   = "call " ++ fnname f -- ++ " (defined at " ++ filename f ++ ":" ++ show (fileline f) ++ ")"
  show (Ret)       = "ret"
  show (Lidt r)    = "lidt " ++ show r
  show (ShL i r)   = "shl " ++ show i ++ " " ++ show r
  show (ShR i r)   = "shr " ++ show i ++ " " ++ show r
  show (And w r)   = "and 0x" ++ (showHex w "") ++ " " ++ show r
  show (Or r1 r2)  = "or " ++ show r1 ++ " " ++ show r2
  show (Enter l)   = "enter 0x" ++ (showHex l "")
  show (Leave)     = "leave"

