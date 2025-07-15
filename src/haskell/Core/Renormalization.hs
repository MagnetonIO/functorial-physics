{-# LANGUAGE TupleSections #-}

module FunctorialRenormalization where

import Control.Monad (join)

----------------------------------------------------------------
-- 1. Data Type: FieldConfig
--    Represents a toy "field configuration" with an energy scale
--    and a list of couplings.
----------------------------------------------------------------

data FieldConfig = FieldConfig
  { energyScale  :: Double
  , couplings    :: [Double]
  } deriving (Eq, Show)

----------------------------------------------------------------
-- 2. RenormFunctor
--    A renormalization functor is simply a function
--    that maps one FieldConfig to another.
----------------------------------------------------------------

type RenormFunctor = FieldConfig -> FieldConfig

----------------------------------------------------------------
-- 3. renormStep
--    A simple "renormalization step" that lowers the energy scale
--    and modifies couplings accordingly (toy example).
----------------------------------------------------------------

renormStep :: Double -> RenormFunctor
renormStep newScale fc =
  let oldScale = energyScale fc
      scaleRatio =
        if oldScale == 0 then 1.0 else (newScale / oldScale)
      newCouplings =
        map (\g -> g * scaleRatio) (couplings fc)
  in FieldConfig
       { energyScale = newScale
       , couplings   = newCouplings
       }

----------------------------------------------------------------
-- 4. composeRenorm
--    Composes two renorm steps in typical Haskell fashion.
----------------------------------------------------------------

composeRenorm :: Double -> Double -> FieldConfig -> FieldConfig
composeRenorm eMid eLow =
  let stepMid = renormStep eMid
      stepLow = renormStep eLow
  in stepLow . stepMid

----------------------------------------------------------------
-- 5. counterterm
--    A simplistic "counterterm" that clips couplings
--    exceeding a threshold.
----------------------------------------------------------------

counterterm :: Double -> FieldConfig -> FieldConfig
counterterm threshold fc =
  let adjCouplings =
        map (\g ->
               if abs g > threshold
               then threshold * signum g
               else g
            )
            (couplings fc)
  in fc { couplings = adjCouplings }

----------------------------------------------------------------
-- 6. demoRenorm
--    Demonstrates a sequence of renormalization steps and
--    counterterms.
----------------------------------------------------------------

demoRenorm :: IO ()
demoRenorm = do
  let initFC = FieldConfig { energyScale = 100.0, couplings = [1.0, 2.0, 3.0] }
  putStrLn ("Initial Field Config: " ++ show initFC)

  let fcMid = renormStep 50.0 initFC
  putStrLn ("After first step (100 -> 50): " ++ show fcMid)

  let fcMidCT = counterterm 2.5 fcMid
  putStrLn ("After counterterm: " ++ show fcMidCT)

  let fcLow = renormStep 10.0 fcMidCT
  putStrLn ("After second step (50 -> 10): " ++ show fcLow)

  let fcLowCT = counterterm 1.5 fcLow
  putStrLn ("Final couplings with counterterm: " ++ show fcLowCT)
