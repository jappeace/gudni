module Graphics.Gudni.Raster.TileTree
  ( TileTree(..)
  , Tile(..)
  , buildTileTree
  , addPrimToTree
  , tileTreeToList
  )
where

import Graphics.Gudni.Raster.Constants
import Graphics.Gudni.Figure
import Graphics.Gudni.Raster.TraverseShapeTree
import Graphics.Gudni.Raster.Enclosure
import Graphics.Gudni.Util.Debug

import Control.Lens

type Width  = Ortho XDimension DisplaySpace
type Height = Ortho YDimension DisplaySpace
type Size   = Int

data Tile = Tile
    { tilePrims       :: [PrimEntry]
    , tileStrandCount :: NumStrands
    , tilePrimCount   :: Size
    , tileBox         :: Box IntSpace
    } deriving (Show)

type TileTree = HTree

data HTree = HTree
    { hCut        :: Width
    , leftBranch  :: VTree
    , rightBranch :: VTree
    }
    | HLeaf Tile
    deriving (Show)

data VTree = VTree
    { vCut         :: Height
    , topBranch    :: HTree
    , bottomBranch :: HTree
    }
    | VLeaf Tile
    deriving (Show)

buildTileTree :: Point2 IntSpace -> HTree
buildTileTree = buildTileTree' (fromIntegral <$> mAXtILEsIZE)

adjustedLog x = if x < 1 then 0 else ceiling . logBase 2 . fromIntegral $ x

buildTileTree' :: Point2 IntSpace -> Point2 IntSpace -> HTree
buildTileTree' tileSize canvasSize = goH maxDepth (pointToBox canvasSize)
    where
    gridW = canvasSize ^. pX `div` tileSize ^. pX
    gridH = canvasSize ^. pY `div` tileSize ^. pY
    logWidth  = adjustedLog gridW
    logHeight = adjustedLog gridH
    maxDepth  = max logWidth logHeight
    goH depth box =
      let hIntCut = max (canvasSize ^. pX) (box ^. leftSide + fromIntegral (2 ^ (depth - 1)))
          hCut = fromIntegral hIntCut
      in  if depth > 0
          then HTree hCut (goV depth (set rightSide hIntCut box))
                          (goV depth (set leftSide  hIntCut box))
          else HLeaf $ emptyTile box
    goV depth box =
      let vIntCut = max (canvasSize ^. pY) (box ^. topSide + fromIntegral (2 ^ (depth - 1)))
          vCut = fromIntegral vIntCut
      in  VTree vCut (goH (depth - 1) (set bottomSide vIntCut box))
                     (goH (depth - 1) (set topSide    vIntCut box))

addPrimToTree :: HTree -> PrimEntry -> HTree
addPrimToTree = insertPrimH

emptyTile :: Box IntSpace -> Tile
emptyTile box = Tile [] 0 0 box

insertPrimH :: HTree -> PrimEntry -> HTree
insertPrimH (HTree cut left right) primEntry =
    let left'  = if primEntry ^. primBox . leftSide < cut
                 then insertPrimV left primEntry
                 else left
        right' = if primEntry ^. primBox .rightSide > cut
                 then insertPrimV right primEntry
                 else right
    in  HTree cut left' right'
insertPrimH (HLeaf tile) primEntry =
  if checkTileSpace tile primEntry
  then HLeaf $ insertPrimTile tile primEntry
  else if widthBox (tileBox tile) > mINtILEsIZE ^. pX
       then insertPrimH (hSplit tile) primEntry
       else (HLeaf tile) -- once the tile is too small to split, start ignoring shapes.

insertPrimV :: VTree -> PrimEntry -> VTree
insertPrimV (VTree cut top bottom) primEntry =
    let top'    = if primEntry ^. primBox . topSide < cut
                  then insertPrimH top primEntry
                  else top
        bottom' = if primEntry ^. primBox . bottomSide > cut
                  then insertPrimH bottom primEntry
                  else bottom
    in  VTree cut top' bottom'
insertPrimV (VLeaf tile) primEntry =
    if checkTileSpace tile primEntry
    then VLeaf $ insertPrimTile tile primEntry
    else if heightBox (tileBox tile) > mINtILEsIZE ^. pY
         then insertPrimV (vSplit tile) primEntry
         else (VLeaf tile) -- once the tile is too small to split, start ignoring shapes.

insertPrimTile :: Tile -> PrimEntry -> Tile
insertPrimTile tile primEntry =
  Tile { tilePrims = primEntry:tilePrims tile
       , tileStrandCount = tileStrandCount tile + primEntry ^. primStrandCount
       , tilePrimCount = tilePrimCount tile + 1
       , tileBox = tileBox tile
       }

checkTileSpace :: Tile -> PrimEntry -> Bool
checkTileSpace tile primEntry = tilePrimCount tile < mAXsHAPE && (tileStrandCount tile + (primEntry ^. primStrandCount)) < (NumStrands . fromIntegral $ (mAXtHRESHOLDS `div` 4))

hSplit :: Tile -> HTree
hSplit tile =
  let box = tileBox tile
      cut = box ^. leftSide + (widthBox box `div` 2)
      lBox = set rightSide cut box
      rBox = set leftSide  cut box
      lTile = emptyTile lBox
      rTile = emptyTile rBox
      hTree = HTree (fromIntegral cut) (VLeaf lTile) (VLeaf rTile)
  in  foldl insertPrimH hTree (tilePrims tile)

vSplit :: Tile -> VTree
vSplit tile =
  let box = tileBox tile
      cut = box ^. topSide + (heightBox box `div` 2)
      tBox = set bottomSide cut box
      bBox = set topSide    cut box
      tTile = emptyTile tBox
      bTile = emptyTile bBox
      vTree = VTree (fromIntegral cut) (HLeaf tTile) (HLeaf bTile)
  in  foldl insertPrimV vTree (tilePrims tile)

tileTreeToList :: TileTree -> [Tile]
tileTreeToList = tileTreeToListH

tileTreeToListH :: HTree -> [Tile]
tileTreeToListH (HTree _ left right) = tileTreeToListV left ++ tileTreeToListV right
tileTreeToListH (HLeaf tile) = pure tile

tileTreeToListV :: VTree -> [Tile]
tileTreeToListV (VTree _ top bottom) = tileTreeToListH top ++ tileTreeToListH bottom
tileTreeToListV (VLeaf tile) = pure tile
