{-# LANGUAGE
	NoImplicitPrelude,
	Rank2Types,
	FlexibleContexts,
	TypeFamilies
	#-}
module Potential.IxMonad.Region
	( IxRegionT(..), Region, RegionMgr(..)
	, SubRegion(..), inSupRegion
	, withRegion, nestRegion
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

withRegion :: ( IxMonad m
	      , Composition ct Unmodeled, Compose ct Unmodeled ~ ct
	      , Composition Unmodeled ct, Compose Unmodeled ct ~ ct )
	   => RegionMgr m
	   -> (forall r . Region typ r m ct x y a)
	   -> m ct x y a
withRegion region r =
     do enter region
	a <- runIxReaderT (runIxRegionT r) region
	close region
	return a

newtype SubRegion typ r s m ct x y =
 SubRegion (forall a .
	     Region typ r m ct x y a ->
	     Region typ s m ct x y a)

inSupRegion :: SubRegion typ r s m ct x y
	    -> Region typ r m ct x y a
	    -> Region typ s m ct x y a
inSupRegion (SubRegion sr) r = sr r

nestRegion :: ( IxMonad m
	      , Composition Unmodeled ct, Compose Unmodeled ct ~ ct
	      , Composition ct Unmodeled, Compose ct Unmodeled ~ ct )
	   => (forall s . SubRegion typ r s m ct x y
			    -> Region typ s m ct x y a)
	   -> Region typ r m ct x y a
nestRegion body = IxRegionT $
     do region <- ask
	let witness (IxRegionT m) = lift $
	     do goUp region
		a <- runIxReaderT m region
		comeDown region
		return a
	lift $ withRegion region (body $ SubRegion witness)

