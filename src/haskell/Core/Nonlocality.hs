{-# LANGUAGE TupleSections #-}

module FunctorialNonlocality where

import Control.Monad (join)

----------------------------------------------------------------
-- 1. Basic Category (->) in Haskell
----------------------------------------------------------------

-- In Haskell, (->) is already a Category:
--   id   :: a -> a
--   (.)  :: (b -> c) -> (a -> b) -> a -> c
--
-- We will use (->) as our base category of morphisms.

----------------------------------------------------------------
-- 2. Probability Distributions & Qubits
----------------------------------------------------------------

-- A simplified probability distribution type for demonstration.
type Prob a = [(a, Double)]
-- We assume the list covers possible outcomes with associated probabilities.

-- A toy Qubit type.
data Qubit = Zero | One
  deriving (Eq, Show)

-- For convenience, let's normalize a probability distribution manually.
normalize :: Prob a -> Prob a
normalize xs = 
  let s = sum (map snd xs)
  in if s == 0 then [] else map (\(x,p) -> (x, p / s)) xs

----------------------------------------------------------------
-- 3. States as Morphisms:  () -> A  (i.e., Unit -> System)
--
--   In category theory, a "state" of system A can be viewed as
--   a morphism from the monoidal unit I (here modeled as `()`)
--   to the object A (e.g., a Hilbert space or, in our naive 
--   example, a distribution).
----------------------------------------------------------------

-- We'll treat a "probabilistic state" of a system A 
-- as a Haskell function:  () -> Prob A
type State a = () -> Prob a

----------------------------------------------------------------
-- 4. Composite System:  A ⊗ B
--
--   In a genuine monoidal category, we would define ⊗ with
--   strict axioms. Here we approximate "tensor" with a 
--   direct product type (A,B).
----------------------------------------------------------------

-- We define a composite system as a pair (A,B) for demonstration:
type Composite a b = (a, b)

-- A "state" of the composite system is then: () -> Prob (a, b).

----------------------------------------------------------------
-- 5. An Entangled "Bell State" in Our Toy Model
--
--   Real quantum Bell states live in C^2 ⊗ C^2, but we'll 
--   encode them as a probability distribution with perfect 
--   correlations. For instance, (Zero,Zero) or (One,One).
----------------------------------------------------------------

bellState :: State (Qubit, Qubit)
bellState () =
  normalize
    [ ((Zero,Zero), 0.5)
    , ((One, One ), 0.5)
    ]

{- 
  This distribution effectively says:
     P(Zero,Zero) = 0.5
     P(One, One ) = 0.5
  There's no classical factorization into "state of A" x "state of B" 
  that reproduces these correlations for all possible measurements.
  In a real quantum formalism, you'd have complex amplitudes and 
  superpositions, but this is a toy example capturing the idea 
  that the global state does not factor into separate local states.
-}

----------------------------------------------------------------
-- 6. Factorization Check
--
--   A state S: () -> Prob (a, b) is "factorizable" if we can 
--   write it as (S_a × S_b) for two states S_a: () -> Prob a 
--   and S_b: () -> Prob b. This is a naive check for "product-ness."
----------------------------------------------------------------

-- A naive attempt to factor a bipartite distribution:
factorize 
  :: State (a,b) 
  -> Maybe (State a, State b)
factorize sAB = do
  -- Attempt to find distributions S_a, S_b such that
  -- S_ab() = (S_a() x S_b()) in a consistent way.
  --
  -- We'll do a simplistic approach:
  --   1. Marginalize over A and B.
  --   2. If p(a,b) = pA(a) * pB(b) for all (a,b), succeed.
  
  let distAB = sAB ()
      distA = marginalA distAB
      distB = marginalB distAB
  
  -- check consistency:
  let isConsistent = all (\((a,b), pAB) ->
                            approxEqual pAB (pA a * pB b))
                          distAB
        where
          pA x = lookupProb x distA
          pB y = lookupProb y distB
  
  if isConsistent
    then Just (const distA, const distB)
    else Nothing
  where
    marginalA :: Prob (a,b) -> Prob a
    marginalA xs =
      normalize $ 
        foldr (\((a,_),p) acc -> (a,p):acc) [] xs

    marginalB :: Prob (a,b) -> Prob b
    marginalB xs =
      normalize $
        foldr (\((_,b),p) acc -> (b,p):acc) [] xs
    
    lookupProb x dist = maybe 0 id (lookup x dist)
    approxEqual x y   = abs (x - y) < 1e-7

----------------------------------------------------------------
-- 7. Example: Checking if bellState is factorizable
----------------------------------------------------------------

checkEntanglement :: IO ()
checkEntanglement = do
  putStrLn "Checking if the bellState is factorizable..."
  case factorize bellState of
    Nothing -> putStrLn "=> The bellState cannot be factorized (Entangled!)."
    Just _  -> putStrLn "=> The bellState factorizes (Product state)."

----------------------------------------------------------------
-- 8. Measurement as a Functor (Toy Example)
--
--   In a real categorical setting, a "Measurement" can be 
--   seen as a functor from (Quantum objects) to (Classical data).
--   Here, we model a measurement as a function from Prob (Qubit)
--   to classical outcomes with updated distributions.
----------------------------------------------------------------

-- We'll define a simplistic "measureFirstQubit" that returns
-- the outcome of the first qubit, collapsing the pair distribution
-- accordingly.
measureFirstQubit :: Prob (Qubit, Qubit) -> Prob (Qubit, (Qubit, Qubit))
measureFirstQubit distAB =
  -- For each (qA,qB), we produce an outcome "qA" with probability p
  -- and keep track of the post-measurement pair if needed. 
  -- This is a naive classical measurement approach.
  [ ( (qA, (qA,qB)), p ) | ((qA,qB), p) <- distAB ]

runMeasurementExample :: IO ()
runMeasurementExample = do
  let distAB = bellState ()
  putStrLn "Initial Bell distribution for (Qubit,Qubit):"
  print distAB
  let measured = normalize (measureFirstQubit distAB)
  putStrLn "\nAfter measuring the first qubit (toy model):"
  mapM_ print measured
  {- 
     We might see that with the "Bell distribution", 
     measuring the first qubit 'Zero' collapses the pair 
     to (Zero,Zero), and measuring 'One' collapses the pair 
     to (One,One).
  -}

----------------------------------------------------------------
-- 9. Main for demo
----------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "=== Demonstration: Functorial Nonlocality (Toy) ==="
  checkEntanglement
  putStrLn ""
  runMeasurementExample
