{-# LANGUAGE
	TemplateHaskell #-}
module Potential.Arch.Builder (defineRegisters) where

import Prelude
import Language.Haskell.TH

import Potential.Arch.SetGet

dataName reg = mkName $ "RR" ++ reg
regName reg = mkName $ "R" ++ reg
argName reg = mkName $ "r" ++ reg
argName' reg = mkName $ "r" ++ reg ++ "'"
msArgName reg = mkName $ "ms_r" ++ reg

defRegisterData name =
        [ DataD []
		(dataName $ name)
		[]
		[ NormalC (dataName $ name) [] ]
		[mkName "Show"]
	]

defRegisterName name =
         [ ValD (VarP $ argName name)
		(NormalB (ConE $ dataName name))
	   []
	 ]

defArgInstance regtyp name =
    [ InstanceD []
		(foldl AppT (ConT ''IsArg) [ConT $ dataName name, ConT regtyp])
		[ FunD 'arg
		       [ Clause [ConP (dataName name) []]
				(NormalB (ConE $ regName name))
				[]
		       ]
		, FunD 'isArg
		       [ Clause [ConP (dataName name) []]
				(NormalB (ConE $ mkName "()"))
				[]
		       ]
		]
    ]

replace _ _ [] = []
replace old new (a:as) = if old == a then new : replace old new as
				     else a : replace old new as

defSetInstance typ regs name =
 let ms  = foldl AppT (ConT typ) (map (VarT . argName) regs)
     ms' = foldl AppT (ConT typ)
		 (map VarT (replace (argName name)
				    (argName' name)
				    (map argName regs)))
 in [ InstanceD []
		(foldl AppT (ConT ''Setter) $
			[ ConT $ dataName name
			, VarT $ argName' name
			, ms
			]
		)
		[ TySynInstD ''Set
				[ ConT $ dataName name
				 , VarT $ argName' name
				 , ms
				]
				ms'
		, FunD 'set'
		       [ Clause [ ConP (dataName name) []
				, VarP (argName' name)
				, VarP (mkName "ms")
				]
				(NormalB
				 (RecUpdE
				  (VarE $ mkName "ms")
				   [(msArgName name, VarE $ argName' name)]
				 )
				)
				[]
		       ]
		]
    ]

defGetInstance typ regs name =
 let ms  = foldl AppT (ConT typ) (map (VarT . argName) regs)
 in [ InstanceD []
		(foldl AppT (ConT ''Getter)
			[ ConT $ dataName name, ms ]
		)
		[ TySynInstD ''Get
				[ ConT $ dataName name, ms ]
				(VarT $ argName name)
		, FunD 'get'
		       [ Clause [ ConP (dataName name) [] ]
				(NormalB (VarE $ msArgName name))
				[]
		       ]
		]
    ]

defineRegisters mstyp regtyp regs =
    return $ concat $ (map defRegisterData regs) ++
		      (map defRegisterName regs) ++
		      (map (defArgInstance regtyp) regs) ++
		      (map (defSetInstance mstyp regs) regs) ++
		      (map (defGetInstance mstyp regs) regs)

