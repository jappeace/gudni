{-# LANGUAGE GADTs            #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Graphics.Gudni.Figure.OpenCurve
  ( OpenCurve(..)
  , curveSegments
  , terminator
  , outset
  , (<^>)
  , reverseCurve
  , overCurve
  )
where

import Graphics.Gudni.Figure.Space
import Graphics.Gudni.Figure.Point
import Graphics.Gudni.Figure.Segment
import Graphics.Gudni.Figure.Angle
import Graphics.Gudni.Figure.Transformer

import Graphics.Gudni.Util.Debug

import Data.Maybe
import Data.Fixed
import Data.Hashable
import qualified Data.Map as M

import Control.Monad.State
import Control.DeepSeq
import Control.Applicative
import Control.Lens

-- | An (almost) typesafe representation of an open bezier curve.
data OpenCurve s = OpenCurve
    { _curveSegments :: [Segment s]
    , _terminator    :: (Point2 s)
    } deriving (Show, Eq, Ord)
makeLenses ''OpenCurve

outset :: Lens' (OpenCurve s) (Point2 s)
outset elt_fn (OpenCurve segments terminator) =
  let (Seg p mc) = head segments
  in  (\p' -> OpenCurve (Seg p' mc:tail segments) terminator) <$> elt_fn p

overCurve :: (Point2 s -> Point2 z) -> OpenCurve s -> OpenCurve z
overCurve f (OpenCurve segs term) = OpenCurve (map (overSegment f) segs) (f term)

pullSegments :: Segment s -> Segment s -> Segment s
pullSegments (Seg o0 c0) (Seg o1 c1) = Seg o1 c0

acrossPairs f (a:b:cs) = f a b : acrossPairs f (b:cs)
acrossPairs f (a:[]) = []
acrossPairs f [] = error "list must have on or more elements"

reverseCurve :: OpenCurve s -> OpenCurve s
reverseCurve (OpenCurve segments terminator) =
  let terminal = head segments ^. anchor
      extendSegments = segments ++ [Seg terminator Nothing]
      revCurve = reverse . acrossPairs pullSegments $ segments
  in  OpenCurve revCurve terminal

-- | Connect to curves end to end by translating c1 so that the starting point of 'c1' is equal to the terminator of 'c2'
(<^>) :: Num s => OpenCurve s -> OpenCurve s -> OpenCurve s
(<^>) c0 c1 = let delta = c0 ^. terminator ^-^ c1 ^. outset
                  transC1 = overCurve (tTranslate delta) c1
              in  over curveSegments (c0 ^. curveSegments ++) transC1

instance Hashable s => Hashable (OpenCurve s) where
    hashWithSalt s (OpenCurve segs term) = s `hashWithSalt` segs `hashWithSalt` term

instance NFData s => NFData (OpenCurve s) where
    rnf (OpenCurve a b) = a `deepseq` b `deepseq` ()
