{-# LANGUAGE
	TemplateHaskell #-}
module Potential.MachineStateBuilder (defineRegisters) where

import Language.Haskell.TH

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

defArgInstance name =
    [ InstanceD []
		(AppT (ConT $ mkName "MSArg") (ConT $ dataName name))
		[ FunD (mkName "arg")
		       [ Clause [ConP (dataName name) []]
				(NormalB (ConE $ regName name))
				[]
		       ]
		, FunD (mkName "isArg")
		       [ Clause [ConP (dataName name) []]
				(NormalB (ConE $ mkName "()"))
				[]
		       ]
		]
    ]

replace _ _ [] = []
replace old new (a:as) = if old == a then new : replace old new as
				     else a : replace old new as

defMSSetInstance regs name =
 let ms  = foldl AppT (ConT $ mkName "MS") (map (VarT . argName) regs)
     ms' = foldl AppT (ConT $ mkName "MS")
		 (map VarT (replace (argName name)
				    (argName' name)
				    (map argName regs)))
 in [ InstanceD []
		(foldl AppT (ConT $ mkName "MSSet") $
			[ ConT $ dataName name
			, VarT $ argName' name
			] ++
			(map (VarT . argName) regs)
		)
		[ TySynInstD (mkName "Set")
				([ ConT $ dataName name
				 , VarT $ argName' name
				 ] ++
				 (map (VarT . argName) regs)
				)
				ms'
		, FunD (mkName "set'")
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

defMSGetInstance regs name =
 let ms  = foldl AppT (ConT $ mkName "MS") (map (VarT . argName) regs)
 in [ InstanceD []
		(foldl AppT (ConT $ mkName "MSGet") $
			[ ConT $ dataName name ] ++
			(map (VarT . argName) regs)
		)
		[ TySynInstD (mkName "Get")
				([ ConT $ dataName name ] ++
				 (map (VarT . argName) regs)
				)
				(VarT $ argName name)
		, FunD (mkName "get'")
		       [ Clause [ ConP (dataName name) [] ]
				(NormalB (VarE $ msArgName name))
				[]
		       ]
		]
    ]

defineRegisters regs =
    return $ concat $ (map defRegisterData regs) ++
		      (map defRegisterName regs) ++
		      (map defArgInstance regs) ++
		      (map (defMSSetInstance regs) regs) ++
		      (map (defMSGetInstance regs) regs)

