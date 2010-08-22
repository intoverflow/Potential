{-# LANGUAGE
	TypeFamilies,
	TemplateHaskell,
	Rank2Types,
	FlexibleContexts #-}
module Language.Potential.Functions
	( defun, renderFn, asm
	, getType, getTypeOf
	) where

import Prelude
import Language.Potential.Core hiding (return, (>>), (>>=), fail)
import Language.Potential.Printing

{-
pos = do loc <- TH.location
	 let filename = TH.loc_filename loc
	     fileline = fst $ TH.loc_start loc
	 p <- [| (filename, fileline) :: (String, Int) |]
	 return p
-}

defun :: IxCode m
	=> String
	-> m Terminal assumes returns ()
	-> Function m assumes returns
defun fnname c =
     Fn { fnname = fnname, body = c }

-- renderFn :: (ASMable m i, Show i) => Function m assumes returns -> IO ()
renderFn c =
     let asmcode = (asm ConstraintsOff $ body c) :: [Instr]
     in unlines $ [fnname c ++ ":"] ++
		  (map (\l -> "    " ++ show l) asmcode)


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

getTypeOf :: src -> Function (Code ConstraintsOn) ms' ms -> Get src ms
getTypeOf src fn = undefined

