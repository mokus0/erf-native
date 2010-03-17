module Data.Number.Erf where

import qualified GSL.SpecFunc.Erf as Impl

-- The Erf class declaration is a verbatim copy from the erf package,
-- written by Lennart Augustsson

-- |Error function related functions.
--
-- The derivative of 'erf' is @\ x -> 2 / sqrt pi * exp (x^2)@,
-- and this uniquely determines 'erf' by @erf 0 = 0@.
--
-- Minimal complete definition is 'erfc' or 'normcdf'.
class (Floating a) => Erf a where
    erf :: a -> a
    erfc :: a -> a       -- ^@erfc x = 1 - erf x@
    erfcx :: a -> a      -- ^@erfcx x = exp (x*x) * erfc x@
    normcdf :: a -> a    -- ^@normcdf x = erfc(-x / sqrt 2) / 2@

    -- All the functions are inter-related, here's some defaults.
    erf x = 1 - erfc x
    erfc x = 2 * normcdf (-x * sqrt 2)
    erfcx x = exp (x*x) * erfc x
    normcdf x = erfc(-x / sqrt 2) / 2

instance Erf Double where
    erf  = Impl.erf
    erfc = Impl.erfc

instance Erf Float where
    erf  = Impl.erff
    erfc = Impl.erfcf
