{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Raster.TraverseShapeTree
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Functions for traversing a ShapeTree while accumulating metadata from the
-- transform and meld nodes along the way.

module Graphics.Gudni.Raster.TraverseShapeTree
  ( traverseTree
  , traverseCompoundTree
  , traverseShapeTree
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Raster.Types

import Control.Lens

-- | Operate on a compound junction.
traverseCompound :: Compound -> Compound -> (Compound, Compound)
traverseCompound CompoundAdd      current = (current, current)
traverseCompound CompoundSubtract current = (current, invertCompound current)
traverseCompound CompoundContinue current = (current, current)

-- | Operate on an overlay junction.
traverseUnit :: () -> () -> ((), ())
traverseUnit () () = ((),())

-- | Traverse across an STree monadically collecting metadata from
traverseTree :: (Monad m, HasDefault o, HasDefault t)
             => (o -> o -> (o, o))
             -> (t -> t -> t)
             -> o
             -> t
             -> (o -> t -> rep -> m ())
             -> STree o t rep
             -> m ()
traverseTree combineOp transformOp c t f tree = go c t tree
    where go c t tree =
              case tree of
                  SLeaf rep -> f c t rep
                  SMeld overlap above below ->
                     do let (a, b) = combineOp overlap c
                        go b t below
                        go a t above
                  STransform tOp child -> go c (transformOp tOp t) child

-- | Traverse a compound shape tree
traverseCompoundTree :: (Num s, Monad m)
                     => Compound
                     -> Transformer s
                     -> (Compound -> Transformer s -> rep -> m ())
                     -> STree Compound (Transformer s) rep
                     -> m ()
traverseCompoundTree o t = traverseTree traverseCompound CombineTransform o t

-- | Traverse an overlap shape tree
traverseShapeTree :: (Num s, Monad m)
                  => (() -> Transformer s -> rep -> m ())
                  -> STree () (Transformer s) rep
                  -> m ()
traverseShapeTree = traverseTree traverseUnit CombineTransform () identityTransform
