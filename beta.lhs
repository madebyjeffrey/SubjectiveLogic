> {-# LANGUAGE ExistentialQuantification #-}
> module Beta (beta) where

> import Data.Ratio
> import Data.Maybe
> import Data.Monoid
> import Control.Applicative

> import SubjectiveLogic

Generating a Beta Mapping: (eq 3.6)

> data Beta = forall b. Fractional b => Beta b b b b


> beta :: Opinion -> Beta
> beta w (Opinion b d 0 a) = Beta (1/0) (1/0) (fromRational a)
> beta w (Opinion b d u a) = Beta r s a
>   where
>       r = fromRational (w * b / u)
>       s = fromRational (w * d / u)


