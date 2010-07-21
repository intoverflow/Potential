{-# LANGUAGE
	TypeFamilies,
	TemplateHaskell,
	Rank2Types,
	FlexibleContexts #-}
module Potential.Functions
	( asCode, renderFn, asm
	, getType, getTypeOf
	) where

import Language.Haskell.TH

import Potential.Core hiding (return, (>>), (>>=), fail)
import Potential.Printing

pos = do loc <- location
	 let filename = loc_filename loc
	     fileline = fst $ loc_start loc
	 p <- [| (filename, fileline) :: (String, Int) |]
	 return p

asCode :: IxCode m
	=> String
	-> m Terminal assumes returns ()
	-> Function m assumes returns
asCode fnname c =
     Fn { fnname    = fnname
	, body      = c
	}

-- renderFn :: (ASMable m i, Show i) => Function m assumes returns -> IO ()
renderFn c =
     do { let asmcode = (asm ConstraintsOff $ body c) :: [Instr]
	; putStrLn $ fnname c ++ ":"
	-- ; putStrLn $
	--	"    // defined at " ++ filename c ++ ":" ++ show (fileline c)
	; mapM_ (\l -> putStrLn $ "    " ++ show l) asmcode
	}


getType :: Function (Code ConstraintsOn)
		    (MS rax rbx rcx rdx rsi rdi rbp rsp rflags
			rip r08 r09 r10 r11 r12 r13 r14 r15
			alloc cmp)
		    (MS rax' rbx' rcx' rdx' rsi' rdi' rbp' rsp' rflags'
			rip' r08' r09' r10' r11' r12' r13' r14' r15'
			alloc' cmp')
	-> Function (Code ConstraintsOn)
		    (MS rax rbx rcx rdx rsi rdi rbp rsp rflags
			rip r08 r09 r10 r11 r12 r13 r14 r15
			alloc cmp)
		    (MS rax' rbx' rcx' rdx' rsi' rdi' rbp' rsp' rflags'
			rip' r08' r09' r10' r11' r12' r13' r14' r15'
			alloc' cmp')
getType fn = fn

getTypeOf
    :: src
    -> Function (Code ConstraintsOn)
		(MS rax' rbx' rcx' rdx' rsi' rdi' rbp' rsp' rflags'
		    rip' r08' r09' r10' r11' r12' r13' r14' r15' alloc' cmp)
		(MS rax rbx rcx rdx rsi rdi rbp rsp rflags
		    rip r08 r09 r10 r11 r12 r13 r14 r15 alloc cmp)
    -> Get src rax rbx rcx rdx rsi rdi rbp rsp rflags
	       rip r08 r09 r10 r11 r12 r13 r14 r15 alloc cmp
getTypeOf src fn = undefined

