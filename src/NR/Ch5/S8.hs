{-# LANGUAGE RecordWildCards, BangPatterns, FlexibleContexts #-}
-- |Evaluating Chebyshev polynomials, translated from code given
-- in Numerical Recipes, 3d Ed., Section 5.8.
module NR.Ch5.S8 where

import Data.Vector.Generic (Vector, generate, (!))

data Chebyshev v a b = Chebyshev
    { chebOrder     :: Int
    -- ^ Order of the (truncated) approximation, which may be different
    -- from the order of the fitted approximation, which is
    -- @length chebCoeffs - 1@
    , chebCoeffs    :: v b
    , chebA         :: a
    , chebB         :: a
    } deriving (Eq, Show)

chebEval :: (Real a, Fractional b, Vector v b) => Chebyshev v a b -> a -> b
chebEval c = chebEvalM (chebOrder c) c

chebEvalM :: (Real a, Fractional b, Vector v b) => Int -> Chebyshev v a b -> a -> b
chebEvalM order Chebyshev{..} x = go order 0 0 
    where
        go !j !dd !d
            | j > 0     = go (j-1) d (y2*d - dd + (chebCoeffs!j))
            | otherwise = y * d - dd + 0.5 * (chebCoeffs!0)
        
        y  = (2 * realToFrac x - a - b) / (b - a)
        y2 = 2 * y
        
        a = realToFrac chebA
        b = realToFrac chebB
    
    
