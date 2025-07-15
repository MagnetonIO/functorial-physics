{-# LANGUAGE TupleSections #-}

module FunctorialMeasurement where

------------------------------------------------------------
-- 1. Basic Category (->) in Haskell
--    We'll treat (->) as our "category of processes."
------------------------------------------------------------

-- id :: a -> a
-- (.) :: (b -> c) -> (a -> b) -> a -> c
--
-- We'll also define "State" as a function from () to 
-- some distribution or data type.

type Prob a = [(a, Double)]

normalize :: Prob a -> Prob a
normalize xs =
  let s = sum (map snd xs)
  in if s == 0 then [] else map (\(x,p) -> (x, p/s)) xs

------------------------------------------------------------
-- 2. Observers as "Measurement Functors"
--    We'll define a typeclass to illustrate different
--    measurement styles or outcomes.
------------------------------------------------------------

class MeasurementFunctor m where
  -- fromQuantum: from a "quantum system" q to classical data c
  fromQuantum :: q -> m c

-- This is highly abstract; in a real scenario, q might be
-- a wavefunction or density matrix, and m c might be a 
-- probability distribution over classical outcomes c.

------------------------------------------------------------
-- 3. Toy Systems
------------------------------------------------------------
data Qubit = Zero | One
  deriving (Eq, Show)

type State a = () -> a

-- A trivial "quantum" state: 
quantumState :: State Qubit
quantumState () = Zero  -- e.g. always "Zero"

------------------------------------------------------------
-- 4. Observers as Different "Functors"
------------------------------------------------------------

-- A naive measurement: 
-- Observes the Qubit with 50% error or something arbitrary
data NaiveObserver = NaiveObserver

instance MeasurementFunctor NaiveObserver where
  fromQuantum :: Qubit -> Prob Bool
  fromQuantum q = 
    case q of
      Zero -> normalize [ (True, 0.8), (False, 0.2) ]
      One  -> normalize [ (True, 0.3), (False, 0.7) ]

-- Another observer with different probabilities
data DifferentObserver = DifferentObserver

instance MeasurementFunctor DifferentObserver where
  fromQuantum :: Qubit -> Prob (String, Double)
  fromQuantum q =
    case q of
      Zero -> [ (("MeasuredZero",0.0), 1.0) ]
      One  -> [ (("MeasuredOne",1.0), 1.0) ]

------------------------------------------------------------
-- 5. Demonstration: "Measuring" a Qubit from different
--    observer perspectives (functors).
------------------------------------------------------------
measureQubit :: (MeasurementFunctor m) => m -> State Qubit -> IO ()
measureQubit observer st = do
  let qVal = st ()
      classicalData = fromQuantum observer qVal
  putStrLn $ "Quantum value: " ++ show qVal
  putStrLn $ "Classical readout: " ++ show classicalData

main :: IO ()
main = do
  putStrLn "=== Demonstration of Functorial Measurement ==="
  putStrLn "\n[NaiveObserver measuring quantumState]"
  measureQubit NaiveObserver quantumState

  putStrLn "\n[DifferentObserver measuring quantumState]"
  measureQubit DifferentObserver quantumState
