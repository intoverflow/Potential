{-# LANGUAGE
	NoImplicitPrelude,
	Rank2Types,
	FlexibleContexts,
	TypeFamilies
	#-}
module Potential.IxMonad.Region
	( RegionMgr(..), IxRegionT(..), IxSubRegionT(..)
	, Region, SubRegion, SubRegionWitness
	, withRegion, withRegion', nestRegion
	) where

import Prelude( ($), (.) )
import Prelude( undefined )
import Potential.IxMonad.IxMonad
import Potential.IxMonad.Reader


data RegionMgr m =
      RegionMgr { enter :: forall x . m Unmodeled x x ()
		, close :: forall x . m Unmodeled x x ()
		, goUp  :: forall x . m Unmodeled x x ()
		, comeDown :: forall x . m Unmodeled x x ()
		}

newtype IxRegionT typ r m ct x y a =
  IxRegionT { runIxRegionT :: IxReaderT (RegionMgr m) m ct x y a }

type Region typ r m = IxRegionT typ r m

instance IxMonadTrans (IxRegionT typ r) where
  lift f = IxRegionT $ lift f

instance IxFunctor m => IxFunctor (IxRegionT typ r m) where
  fmap f (IxRegionT m) = IxRegionT $ fmap f m

instance IxMonad m => IxMonad (IxRegionT typ r m) where
  unsafeReturn a = lift $ unsafeReturn a
  fl >>= f = IxRegionT $ let fl' = runIxRegionT fl
			 in fl' >>= (runIxRegionT . f)

withRegion' region r = runIxReaderT (runIxRegionT r) region

withRegion :: ( IxMonad m
	      , Composition ct Unmodeled, Compose ct Unmodeled ~ ct
	      , Composition Unmodeled ct, Compose Unmodeled ct ~ ct )
	   => RegionMgr m
	   -> (forall r . Region typ r m ct x y a)
	   -> m ct x y a
withRegion region r =
     do enter region
	a <- withRegion' region r
	close region
	return a

data SubRegionWitness typ r s = SubRegionWitness

newtype IxSubRegionT typ r s m ct x y a =
  IxSubRegionT { runIxSubRegionT :: IxReaderT (SubRegionWitness typ r s)
					      (Region typ s m) ct x y a }

type SubRegion typ r m = IxSubRegionT typ r m

instance IxMonadTrans (IxSubRegionT typ r s) where
  lift f = IxSubRegionT $ lift $ lift f

instance IxFunctor m => IxFunctor (IxSubRegionT typ r s m) where
  fmap f (IxSubRegionT m) = IxSubRegionT $ fmap f m

instance IxMonad m => IxMonad (IxSubRegionT typ r s m) where
  unsafeReturn a = lift $ unsafeReturn a
  fl >>= f = IxSubRegionT $ let fl' = runIxSubRegionT fl
			    in fl' >>= (runIxSubRegionT . f)

nestRegion :: ( IxMonad m
	      , Composition Unmodeled ct, Compose Unmodeled ct ~ ct
	      , Composition ct Unmodeled, Compose ct Unmodeled ~ ct )
	   => (forall s . SubRegion typ r s m ct x y a)
	   -> Region typ r m ct x y a
nestRegion body = IxRegionT $
     do region <- ask
	lift $ withRegion region
			  (runIxReaderT (runIxSubRegionT body) SubRegionWitness)

