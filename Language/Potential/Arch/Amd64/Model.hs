{-# LANGUAGE
	TemplateHaskell,
	MultiParamTypeClasses,
	FlexibleContexts,
	FlexibleInstances,
	TypeFamilies #-}
module Language.Potential.Arch.Amd64.Model
	( Reg(..), MS
	, rax, rbx, rcx, rdx, rsi, rdi, rbp, rsp, rflags
	, rip, r08, r09, r10, r11, r12, r13, r14, r15
	, rcmp, ralloc
	, amd64
	) where

import Prelude
import Language.Potential.Arch.Builder

import Language.Haskell.TH.Quote
import qualified Language.Haskell.TH as TH
import Text.ParserCombinators.Parsec
import Data.Maybe (isNothing)

import Language.Potential.Arch.ParseType
import Language.Potential.Arch.CommonQQ

data Reg =
    Rax | Rbx | Rcx | Rdx
  | Rsi | Rdi | Rbp | Rsp
  | Rflags | Rip
  | R08 | R09 | R10 | R11
  | R12 | R13 | R14 | R15
  | Rcmp | Ralloc -- a hack


data MS rax rbx rcx rdx rsi rdi rbp rsp rflags
	rip r08 r09 r10 r11 r12 r13 r14 r15 alloc cmp =
     MS { ms_rax :: rax	-- return value
	, ms_rbx :: rbx	-- caller
	, ms_rcx :: rcx	-- arg 3
	, ms_rdx :: rdx	-- arg 2
	, ms_rsi :: rsi	-- arg 1
	, ms_rdi :: rdi	-- arg 0
	, ms_rbp :: rbp	-- caller
	, ms_rsp :: rsp
	, ms_rflags :: rflags
	, ms_rip :: rip
	, ms_r08 :: r08	-- arg 4
	, ms_r09 :: r09	-- arg 5
	, ms_r10 :: r10
	, ms_r11 :: r11
	, ms_r12 :: r12	-- caller
	, ms_r13 :: r13	-- caller
	, ms_r14 :: r14	-- caller
	, ms_r15 :: r15	-- caller
	, ms_ralloc :: alloc -- the memory allocator
	, ms_rcmp :: cmp -- the last cmp
	}

defineRegisters ''MS ''Reg
		[ "ax", "bx", "cx", "dx", "si", "di", "bp", "sp", "flags"
		, "ip", "08", "09", "10", "11", "12", "13", "14", "15"
		, "alloc", "cmp"]

parseAmd64 =
     do whiteSpace
	assignments <- commaSep bind
	whiteSpace
	eof
	return $ forgeMS assignments
  where bind =
	     do r <- identifier
		if not (elem r regs)
		  then fail $ "`" ++ r ++ "' is not a register in this model"
		  else do colon
			  t <- parseType parseAmd64 "amd64"
			  return (r, t)

forgeMS assignments =
	TH.ForallT (map (TH.PlainTV . TH.mkName) missing)
		   []
		   (foldl TH.AppT (TH.ConT ''MS) (map assign regs))
  where assign r = maybe (TH.VarT $ TH.mkName r) (id) (lookup r assignments)
	missing = filter (\r -> isNothing $ lookup r assignments) regs

regs =  [ "rax", "rbx", "rcx", "rdx", "rsi", "rdi", "rbp", "rsp"
	, "rflags", "rip"
	, "r08", "r09", "r10", "r11" , "r12", "r13", "r14", "r15"
	, "rcmp", "ralloc" ]

{- Example:
	:t undefined :: $([$amd64| rax : Int |])
-}

amd64 = QuasiQuoter (parseExp $ parseModel parseAmd64)
		    (parsePat $ parseModel parseAmd64)

