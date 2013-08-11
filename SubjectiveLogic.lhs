
> module SubjectiveLogic(BaseRate, Opinion(..), opinion, pp) where

> import Data.Ratio
> import Data.Maybe
> import Data.Monoid
> import Data.Number.Natural
> import Control.Applicative

In Jøsang's subjective logic book (p7), the discussion starts off with the definition of an opinion. An opinion is a tuple containing <degree of belief, degree of uncertainty, and a base rate> over a discrete domain (cardinality of k).

For example, we can have domain:

> class BaseRate a where 
>    baseRate :: a -> Rational

> data Domain = Infected | NotInfected

We must have a base rate function to apply to each element: (definition 2)

> instance BaseRate Domain where
>   baseRate Infected = 1 % 7
>   baseRate NotInfected = 6 % 7



The sum of all base rate functions must be 1. (eq 2.5) In this simplification, we do not have any subsets of the domain. There are two sources for base rates – subjective and empirical [p10]. Subjective rates are determined by (expert?) opinion, and empirical observations.

We are dealing with a binomial opinion, that is X = { x, !x}, this can be represented by

> data Opinion = Opinion { belief :: Rational
>                        , disbelief :: Rational
>                        , uncertainty :: Rational
>                        , baserate :: Rational
>                        }

Just to ensure we can output a representation,

> instance Show Opinion where
>   show (Opinion b d u a) = "ω = (" <> frac b <> ", " <> frac d <> ", " <> frac u <> ", " <> frac a <> ")"
>       where frac x = (show $ numerator x) <> "/" <> (show $ denominator x)

We must ensure that belief + disbelief + uncertainty = 1, and all are within [0, 1]

> opinion :: Rational -> Rational -> Rational -> Rational -> Maybe Opinion
> opinion b d u a = if (and [(all (>= 0) [b,d,u,a]), (all (<= 1) [b,d,u,a]), b+d+u == 1]) then
>                        Just $ Opinion b d u a
>                   else
>                        Nothing

The probability projection is

> pp (Opinion b _ u a) = b + a * u              -- 3.1

A basic example given would be where ω = (0.2, 0.5, 0.3, 0.6), E = 0.38 or (19/50) as shown:

> example1 = pp <$> opinion (2%10) (5%10) (3%10) (6%10)  -- Just (19 % 50)

