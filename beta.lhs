> {-# LANGUAGE ExistentialQuantification, NoMonomorphismRestriction #-}
> module Beta (beta) where

> import Data.Ratio
> import Data.Maybe
> import Data.Monoid
> import Data.List
> import Control.Applicative

import Graphics.Rendering.Chart.Simple
import Graphics.Rendering.Chart.Simple.Internal (PlotPDFType)

> import Graphics.Rendering.Chart

> import SubjectiveLogic
> import Numeric.GSL.Special.Gamma (gamma)

Ignore these?
 data Beta = forall b. (Fractional b, Show b) => Beta b b b 

 instance Show Beta where
   show (Beta r s a) = "Beta(p | " <> (show r) <> "," <> (show s) <> "," <> (show a) <> ")"


 beta :: Rational -> Opinion -> Beta
 beta w (Opinion b d 0 a) = Beta (1/0) (1/0) (fromRational a)
 beta w (Opinion b d u a) = Beta r s a
   where
       r = fromRational (w * b / u)
       s = fromRational (w * d / u)

Generating a Beta Mapping (beta pdf): (eq 3.2, 3.3, 3.6)

> α :: Rational -> Opinion -> Double
> α w (Opinion b _ u a) = fromRational $ w*(b/u + a)

> β :: Rational -> Opinion -> Double
> β w (Opinion _ d u a) = fromRational $ w*(d/u + 1  - a)

> beta :: Double -> Double -> Double -> Double
> beta α β p = gamma (α + β) / ((gamma α)*(gamma β)) * p**(α-1) * (1 - p)**(β-1)

Next we can show a graph:

First we obtain probability values:

> ps :: [Double]
> ps = [0, 0.01 .. 1]

Define the chart for the example beta pdf

> pdfChart = layout
>   where
>       w = 2
>       ω = (opinion (2%10) (5%10) (3%10) (6%10)) 
>       βpdf = (beta (α w o) (β w o))
>       pdf o = plot_lines_style .> line_color ^= opaque blue
>             $ plot_lines_values ^= [ zip ps (map βpdf ps) ]
>             $ plot_lines_title ^= show ω
>             $ default_plot_lines
>
>       layout = layout1_title ^= "Probability Density Function"
>              $ layout1_left_axis ^: laxis_title_ ^= "Density"
>              $ layout1_bottom_axis ^: laxis_title_ ^= "Probability"
>              $ layout1_plots ^= [Left (toPlot pdf (fromJust ω))]
>              $ layout1_grid_last ^= False
>              $ defaultLayout1

> graph :: (PlotPDFType a) => FilePath -> Rational -> Maybe Opinion -> a 
> graph path w (Just o) = plotPDF path probability_range (beta (α w o) (β w o)) Solid
> graph _ _ Nothing = 

fig32 :: (PlotPDFType a) => (Maybe a)
fig32 = graph "betafig32.pdf" 2 <$> (opinion (2%10) (5%10) (3%10) (6%10))  -- figure 3.2