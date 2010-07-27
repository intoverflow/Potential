{-# LANGUAGE
	NoImplicitPrelude,
	Rank2Types,
	FlexibleContexts,
	TypeFamilies
	#-}
module Potential.IxMonad.Region
	( IxMonadRegion(..)
	, RegionMgr(..), IxRegionT(..)
	, Region, SubRegionWitness
	, withRegion, withRegion', nestRegion
	) where

import Prelude( ($), (.) )
import Prelude( undefined )
import Potential.IxMonad.IxMonad
import Potential.IxMonad.Reader

class IxMonad m => IxMonadRegion m where
  type RegionType m
  type RegionLabel m

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

instance IxMonad m => IxMonadRegion (IxRegionT typ r m) where
  type RegionType (IxRegionT typ r m) = typ
  type RegionLabel (IxRegionT typ r m) = r

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

nestRegion :: ( IxMonad m
	      , Composition Unmodeled ct, Compose Unmodeled ct ~ ct
	      , Composition ct Unmodeled, Compose ct Unmodeled ~ ct )
	   => (forall s . IxReaderT (SubRegionWitness typ r s)
				    (Region typ s m) ct x y a)
	   -> Region typ r m ct x y a
nestRegion body = IxRegionT $
     do region <- ask
	lift $ withRegion region (runIxReaderT body SubRegionWitness)

