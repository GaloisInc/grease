{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
-- \|
-- Copyright        : (c) Galois, Inc. 2024
-- Maintainer       : GREASE Maintainers <grease@galois.com>
{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude

main :: IO ()
main = mainWith (pad 1.1 (centerXY plotWithLegend))

type Dia = Diagram B

h, w :: Double
h = 16
w = 16

xAxis :: Dia
xAxis = strokeLine $ lineFromVertices [0 ^& 0, w ^& 0]

yAxis :: Dia
yAxis = strokeLine $ lineFromVertices [0 ^& 0, 0 ^& h]

axes :: Dia
axes =
  yAxis
    === xAxis

byte :: Dia
byte = square 1

yLabels :: [Dia]
yLabels =
  [ alignedText 1 0.5 "0"
  , alignedText 1 0.5 "1"
  , alignedText 1 0.5 "2"
  , alignedText 1 0.5 "3"
  , alignedText 1 0.5 "4"
  ]

xLabels :: [Dia]
xLabels =
  [ text "0x0"
  , text "0x4"
  , text "0x8"
  , text "0xC"
  ]

evenly :: Double -> (Double -> Dia -> Dia) -> Dia -> [Dia] -> Dia
evenly maxCoord trans base ds =
  foldl
    (\accum (i, d) -> trans (i * maxCoord / toEnum (length ds)) d `atop` accum)
    base
    (zip [0 ..] ds)

evenX :: Dia -> [Dia] -> Dia
evenX base ds = evenly w translateX base ds

evenY :: Dia -> [Dia] -> Dia
evenY base ds = evenly h translateY base ds

labeledAxes :: Dia
labeledAxes =
  evenY
    (evenX axes (map (addYGap . rotateBy (1 / 4)) xLabels))
    (map addXGap yLabels)
 where
  addXGap = translateX (-0.5)
  addYGap = translateY (-1.5)

plot :: Dia
plot = evenY labeledAxes (map alignXMin (mempty : allocs))
 where
  allocs = map (\i -> hcat (replicate i byte)) [4, 8, 2, 12]

namedAxes :: Dia
namedAxes =
  rotateBy (1 / 4) (text "Block identifier")
    ||| strutX 2
    ||| ( translateY (-0.5) (centerXY plot)
            === strutY 2
            === alignedText 0.5 1 "Offset"
        )

legend :: Dia
legend = fixCutoff $ pad 1.5 byte ||| alignedText 0 0.5 "= a byte"
 where
  fixCutoff = padX 6.0 -- o/w the text gets cut off

plotWithLegend :: Dia
plotWithLegend =
  translateY (h / 8) (translateX ((w / 2) - (w / 8)) legend)
    `atop` centerXY namedAxes
