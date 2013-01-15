{-# LANGUAGE NoMonomorphismRestriction, TypeFamilies #-}

module Diagrams.Chart.Bar where

import Diagrams.Prelude

data Orientation = Vertical | Horizontal

plotBars orient values = (translate (r2 (0,1)) $ alignB plot) `atop` (alignB background)
   where
      plot = case orient of
         Vertical -> bars === labels
         Horizontal -> labels ||| bars

      bars = drawBars $ map snd values
      labels = drawLabels $ map fst values
      background = rect 40.0 40.0 # bg red

-- | Draw a group of bars
drawBarGroup :: (Int -> Double -> a) -> ([a] -> b) -> [Double] -> b
drawBarGroup makeBar group values = group bars
   where
      indexes = [0,1 ..]
      idxVals = zip indexes values
      bars = map (uncurry makeBar) idxVals

-- | Separator for cumulative bars
cumulativeSep :: (Monoid a, Semigroup a, Juxtaposable a, Alignable a, V a ~ R2) => [a] -> a
cumulativeSep = foldl (===) mempty . reverse

-- | Separator for juxtaposed bars
juxtaposedBarSep :: (Monoid a, HasOrigin a, Alignable a, Juxtaposable a, Semigroup a, V a ~ R2) => Double -> [a] -> a
juxtaposedBarSep dist = cat' (r2 (1,0)) with {sep = dist} . map alignB

-- | Default color serie
defaultColors :: (Floating a, Ord a) => [Colour a]
defaultColors = cycle [red,blue,yellow,green,gray]

-- | Default bar drawing method
defaultRectBar :: (HasStyle a, PathLike a, Transformable a, V a ~ R2) => Int -> Double -> a
defaultRectBar idx size = rect 1.0 size # fc color
   where
      color = defaultColors !! idx

drawBars = drawBarGroup defaultRectBar cumulativeSep --(juxtaposedBarSep 1.0)

drawLabels ls = cat' (r2(1,0)) with {sep = 3.0, catMethod = Distrib} labels
   where
      labels = map makeLabel ls
      makeLabel l = text l # fontSize 0.5
