{-# LANGUAGE TupleSections #-}

module FunctorialCoherenceSymmetry where

import Control.Monad (join)

--------------------------------------------------------------------------------
-- 1. Systems & Symmetries as a Toy Category
--------------------------------------------------------------------------------

-- A 'System' might represent a quantum system, labeled by a string.
-- You can think of it as the "object" in a category.
data System = Sys String
  deriving (Eq, Show)

-- A 'SymEl' might represent an element of a symmetry group, like 'g in G'.
-- We'll store just a label, e.g. "g1", "g2", to demonstrate how it acts.
data SymEl = SymEl String
  deriving (Eq, Show)

-- A 'SymTrans' is a transformation from a system to (the same) system, 
-- labeled by some SymEl. E.g. a gauge transformation, or a global symmetry action.
data SymTrans = SymTrans
  { stIn  :: System
  , stOut :: System
  , stSym :: SymEl
  }
  deriving (Eq, Show)

-- We imagine SymTrans as an "automorphism" of the system stIn -> stOut,
-- where stIn == stOut if it's literally the same system. 
-- We'll not enforce that strictly, just demonstrate the concept.

--------------------------------------------------------------------------------
-- 2. Composition of Symmetry Transformations
--------------------------------------------------------------------------------

-- In a real group, we'd demand stOut of the first == stIn of the second,
-- and stSym composition is group multiplication. Here, we do a naive version.
composeSym :: SymTrans -> SymTrans -> Maybe SymTrans
composeSym s1 s2 =
  if stOut s1 == stIn s2
    then Just SymTrans
         { stIn  = stIn s1
         , stOut = stOut s2
         , stSym = symCombine (stSym s1) (stSym s2)
         }
    else Nothing

-- We'll define a toy "group operation" for SymEl by just concatenating labels.
symCombine :: SymEl -> SymEl -> SymEl
symCombine (SymEl s1) (SymEl s2) = SymEl (s1 ++ "·" ++ s2)

--------------------------------------------------------------------------------
-- 3. Observers / Interpretations as "Functors"
--------------------------------------------------------------------------------

-- We define a typeclass for Observers that "interpret" a system 
-- and a symmetry transformation in some classical or alternative viewpoint.
class Observer obs where
  -- Interpret a 'System' as "classical data" or some vantage type.
  interpretSys :: obs -> System -> String

  -- Interpret a 'SymTrans' (a symmetry action) in the observer's vantage,
  -- returning some label or transformation in the observer's representation.
  interpretSym :: obs -> SymTrans -> String

-- We'll define two different Observers for demonstration.
-- They might represent two different measurement bases or vantage points.

data ObserverA = ObserverA deriving (Show)
data ObserverB = ObserverB deriving (Show)

instance Observer ObserverA where
  interpretSys _ (Sys label)   = "ObsA sees system: " ++ label
  interpretSym _ (SymTrans _ _ (SymEl g)) = "ObsA sees symmetry: [A-" ++ g ++ "]"

instance Observer ObserverB where
  interpretSys _ (Sys label)   = "ObsB system: " ++ label
  interpretSym _ (SymTrans _ _ (SymEl g)) = "ObsB-sym(" ++ g ++ ")"

--------------------------------------------------------------------------------
-- 4. Demo: Checking Coherence Between Observers
--
-- A "coherent" mapping from ObserverA -> ObserverB would require that the
-- interpretation of composed symmetries etc. line up in a consistent way.
--------------------------------------------------------------------------------

-- For simplicity, we define a function that attempts to show how two observers
-- might describe the same transformations, and sees if they "match."

compareInterpretations :: (Observer o1, Observer o2) 
                       => o1 -> o2 -> SymTrans -> SymTrans -> IO ()
compareInterpretations o1 o2 s1 s2 = do
  putStrLn "-- Compare Observers on single transformations --"
  putStrLn $ "O1 sees s1 as: " ++ interpretSym o1 s1
  putStrLn $ "O2 sees s1 as: " ++ interpretSym o2 s1
  putStrLn $ "O1 sees s2 as: " ++ interpretSym o1 s2
  putStrLn $ "O2 sees s2 as: " ++ interpretSym o2 s2

  case composeSym s1 s2 of
    Nothing -> putStrLn "\nNo composition possible (out != in)."
    Just s12 -> do
      putStrLn "\nComposed transformation s1 . s2 => s12"
      putStrLn $ "O1 sees s12: " ++ interpretSym o1 s12
      putStrLn $ "O2 sees s12: " ++ interpretSym o2 s12

--------------------------------------------------------------------------------
-- 5. Main Demo
--------------------------------------------------------------------------------

demo :: IO ()
demo = do
  putStrLn "=== Functorial Re-interpretation of Coherence & Symmetry ===\n"

  -- Create two systems:
  let sysA = Sys "A"
      sysB = Sys "B"

  -- Create two symmetry transformations on these systems
  let trans1 = SymTrans sysA sysA (SymEl "g1")
  let trans2 = SymTrans sysA sysB (SymEl "g2")  -- out is different, may not compose with trans1

  putStrLn $ "trans1: " ++ show trans1
  putStrLn $ "trans2: " ++ show trans2
  putStrLn ""

  -- Compare interpretations from ObserverA & ObserverB
  compareInterpretations ObserverA ObserverB trans1 trans2

  putStrLn "\nNow let's create transformations that do compose..."
  let trans3 = SymTrans sysA sysA (SymEl "g3")
  let trans4 = SymTrans sysA sysA (SymEl "g4")
  putStrLn $ "\ntrans3: " ++ show trans3
  putStrLn $ "trans4: " ++ show trans4
  putStrLn ""
  compareInterpretations ObserverA ObserverB trans3 trans4

  putStrLn "\nDone."

{-
How to use:
1. Save as "FunctorialCoherenceSymmetry.hs"
2. ghc FunctorialCoherenceSymmetry.hs
3. ./FunctorialCoherenceSymmetry

Observe how two different Observers interpret the same 
symmetry transformations. Also see how compositions might
or might not be possible. In a more advanced setting, one 
could define natural transformations bridging ObserverA and 
ObserverB, capturing if they yield consistent data for all 
systems and morphisms.
-}
