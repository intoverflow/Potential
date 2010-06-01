{-# LANGUAGE
	NoImplicitPrelude,
	Rank2Types
	#-}
module Potential.IxMonad.Region
	( IxRegionT(..), Region, RegionMgr(..)
	, withRegion
	, nestRegion
	) where

import Prelude( ($), (.) )
import Prelude( undefined )
import Potential.IxMonad.IxMonad
import Potential.IxMonad.Reader


newtype IxRegionT r mgr m x y ct a =
  IxRegionT { runIxRegionT :: IxReaderT mgr m x y ct a }

instance IxMonadTrans (IxRegionT r mgr) where
  lift f = IxRegionT $ lift f

instance IxMonad m => IxMonad (IxRegionT r mgr m) where
  mixedReturn a = lift $ mixedReturn a
  fl >>>= f = IxRegionT $ let fl' = runIxRegionT fl
			  in fl' >>>= (runIxRegionT . f)

data RegionMgr m =
      RegionMgr { enter :: forall x . m x x Composable ()
		, close :: forall x . m x x Composable ()
		, goUp  :: forall x . m x x Composable ()
		, comeDown :: forall x . m x x Composable ()
		}

type Region r m = IxRegionT r (RegionMgr m) m

withRegion :: IxMonad m
	   => RegionMgr m
	   -> (forall r . Region r m x y Composable a)
	   -> m x y Composable a
withRegion region r =
     do enter region
	a <- runIxReaderT (runIxRegionT r) region
	close region
	return a

newtype SubRegion r s m x y =
 SubRegion (forall a .
	     Region r m x y Composable a -> Region s m x y Composable a)

nestRegion :: IxMonad m
	   => (forall s . SubRegion r s m x y
		-> Region s m x y Composable a)
	   -> Region r m x y Composable a
nestRegion body = IxRegionT $
     do region <- ask
	let witness (IxRegionT m) = lift $
	     do goUp region
		a <- runIxReaderT m region
		comeDown region
		return a
	lift $ withRegion region (body (SubRegion witness))

