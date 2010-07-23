{-# LANGUAGE
	TypeFamilies,
	TemplateHaskell,
	Rank2Types,
	FlexibleContexts #-}
module Potential.Functions
	( defun, renderFn, asm
	, getType, getTypeOf
	) where

import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as THS

import Potential.Core hiding (return, (>>), (>>=), fail)
import Potential.Printing

{-
pos = do loc <- TH.location
	 let filename = TH.loc_filename loc
	     fileline = fst $ TH.loc_start loc
	 p <- [| (filename, fileline) :: (String, Int) |]
	 return p
-}

defun :: IxCode m
	-- => String
	=> m Terminal assumes returns ()
	-> Function m assumes returns
	-- -> TH.Q [TH.Dec]
defun c =
     Fn { body = c }
{-
     do let name = TH.mkName fnname
	loc <- TH.location
	let filename = TH.loc_filename loc
	    fileline = fst $ TH.loc_start loc
	fndef  <- [| Fn { fnname   = fnname
			, body     = c
			, filename = filename
			, fileline = fileline
			} |]
	return [ TH.ValD (TH.VarP name) (TH.NormalB fndef) [] ]
-}

-- renderFn :: (ASMable m i, Show i) => Function m assumes returns -> IO ()
renderFn c =
     do { let asmcode = (asm ConstraintsOff $ body c) :: [Instr]
	; putStrLn $ fnname c ++ ":"
	; putStrLn $
	    "    // defined at " ++ filename c ++ ":" ++ show (fileline c)
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

