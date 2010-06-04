{-# LANGUAGE
	NoImplicitPrelude,
	Rank2Types
	#-}
module Potential.IxMonad.Region
	( IxRegionT(..), Region, RegionMgr(..)
	, SubRegion, inSupRegion
	, withRegion, nestRegion
	) where

import Prelude( ($), (.) )
import Prelude( undefined )
import Potential.IxMonad.IxMonad
import Potential.IxMonad.Reader


data RegionMgr m =
      RegionMgr { enter :: forall x . m x x Composable ()
		, close :: forall x . m x x Composable ()
		, goUp  :: forall x . m x x Composable ()
		, comeDown :: forall x . m x x Composable ()
		}

newtype IxRegionT typ r m x y ct a =
  IxRegionT { runIxRegionT :: IxReaderT (RegionMgr m) m x y ct a }

type Region typ r m = IxRegionT typ r m

instance IxMonadTrans (IxRegionT typ r) where
  lift f = IxRegionT $ lift f

instance IxMonad m => IxMonad (IxRegionT typ r m) where
  mixedReturn a = lift $ mixedReturn a
  fl >>>= f = IxRegionT $ let fl' = runIxRegionT fl
			  in fl' >>>= (runIxRegionT . f)

withRegion :: IxMonad m
	   => RegionMgr m
	   -> (forall r . Region typ r m x y Composable a)
	   -> m x y Composable a
withRegion region r =
     do enter region
	a <- runIxReaderT (runIxRegionT r) region
	close region
	return a

newtype SubRegion typ r s m x y =
 SubRegion (forall a .
	     Region typ r m x y Composable a -> Region typ s m x y Composable a)

inSupRegion :: SubRegion typ r s m x y
	    -> Region typ r m x y Composable a
	    -> Region typ s m x y Composable a
inSupRegion (SubRegion sr) r = sr r

nestRegion :: IxMonad m
	   => (forall s . SubRegion typ r s m x y
		-> Region typ s m x y Composable a)
	   -> Region typ r m x y Composable a
nestRegion body = IxRegionT $
     do region <- ask
	let witness (IxRegionT m) = lift $
	     do goUp region
		a <- runIxReaderT m region
		comeDown region
		return a
	lift $ withRegion region (body (SubRegion witness))

