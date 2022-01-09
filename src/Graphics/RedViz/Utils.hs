{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Graphics.RedViz.Utils
  ( toIdxVAO
  , toIdxVAO'
  , Graphics.RedViz.Utils.fromList
--  , (<$.>)
  , (<*.>)
  , toV3
  , rsl
--  , rotateList'
  , fromUUID
  , encodeStringUUID
  ) where

import Control.Lens ( view )
import Graphics.Rendering.OpenGL as GL (GLfloat)
import Data.ByteString.Char8           (pack
                                       ,unpack)
import Data.Set                  as DS (fromList, toList)
import Data.List.Index                 (indexed)
import Data.List                       (elemIndex)
import Data.Locator
import Data.UUID                 as U
import Data.Vector               as DV (fromList, (!), map, toList)
import Data.VectorSpace          as DV
import Graphics.Rendering.OpenGL (GLuint)
import Linear.V3
import Linear.V4
import Linear.Matrix
import Linear.Metric             as LM
import System.Random

-- import Debug.Trace as DT

newtype V3Double = V3Double (V3 Double)

instance VectorSpace V3Double Double where
  zeroVector                   = V3Double (V3 0 0 0)
  (*^) s (V3Double (V3 x y z)) = V3Double (V3 (s*x) (s*y) (s*z))
  (^+^)  (V3Double (V3 x y z)) (V3Double(V3 k l m)) = V3Double (V3 (x+k) (y+l) (z+m))
  dot    (V3Double (V3 x y z)) (V3Double(V3 k l m)) = (x*k) + (y*l) + (z*m)
  
newtype M44Double = M44Double (V4 (V4 Double))

instance VectorSpace M44Double Double where
  zeroVector                           = M44Double (identity :: M44 Double)
  (*^) s (M44Double (m :: M44 Double)) = M44Double (m !!* s)
  (^+^)  (M44Double (m :: M44 Double)) (M44Double (n :: M44 Double)) =
    M44Double
    (mkTransformationMat
     rot
     tr)
    where
       rot = LM.normalize $ (inv33 m') !*! (n')
         where
           m' = view _m33 m
           n' = view _m33 n
       V3Double tr = V3Double (view translation m) ^+^ V3Double (view translation n)
  dot m n = DV.dot m n

-- | Convrerts a list of lists of floats to an indexed tuple of sets of floats.
-- 
-- Use @toIdxVAO'@ for non-optimzed variant that keeps the duplicates.
toIdxVAO :: [[Float]] -> ([Int],[Float])
toIdxVAO vao = (idx, idxVAO)
  where
    iListSet = indexed $ DS.toList $ DS.fromList $ vao                       :: [(Int,[Float])]
    iList    = indexed vao                                                   :: [(Int, [GLfloat])]
    idx      = fmap fst (matchLists iListSet iList) :: [Int]
    idxVAO   = concat $ fmap (\x -> snd x) iListSet                          :: [Float]

-- | Converts a list of lists of floats to an indexed tuple of list of floats
--
-- This is variant keeps the duplicates, for optimized variant use @toIdxVAO@
toIdxVAO' :: [[Float]] -> ([Int],[Float])
toIdxVAO' vao = (idx, idxVAO)
  where
    --iListSet = indexed $ DS.toList $ DS.fromList $ vao                       :: [(Int,[Float])]
    iList    = indexed vao                             :: [(Int, [GLfloat])]
    idx      = fmap fst iList :: [Int]
    idxVAO   = concat $ fmap (\x -> snd x) iList       :: [Float]

-- | matchLists - cross-match 2 listst, replacing the elements of list2 with matching
--            with elements of list1, concatenating the non-matched elements.
--      il - indexed list
--     nil - non-indexed list
--   mFunc - matching function  
matchLists :: [(Int, [GLfloat])] -> [(Int, [GLfloat])] -> [(Int, [GLfloat])]
matchLists il nil' =
  fmap (mFunc il) nil'
  where
    -- | il      - indexed list
    -- | nile    - non indexed list element
    -- | Replaces the target element with the first match from the matching list il
    -- | if a unique index idx found - flip the sign
                                            -- | the idea idx to separate normal indexes
                                            -- | and unique indexes -> [idx:uidx] later    
    il' = DV.fromList il
    cxs'  = DV.map snd il'
    mFunc _ (iy, cy) =
      (\case
          Just idx -> il' ! idx
          Nothing  -> (-iy, cy) ) nili 
      where
        nili = elemIndex cy (DV.toList cxs')
        -- cxs  = DV.map (\(i,s) -> s) il' -- :: [[GLfloat]]

-- | TODO: create a fromList typeclass?
-- [a] -> V3 a
-- [a] -> M44 a
-- etc.

-- | A convenience function to convert a list of floats into a 4x4 matrix
fromList :: [Float] -> M44 Double
fromList xs' = V4 x y z w
  where
    x  = V4 (xs!!0 ) (xs!!1 ) (xs!!2 ) (xs!!3 ) 
    y  = V4 (xs!!4 ) (xs!!5 ) (xs!!6 ) (xs!!7 ) 
    z  = V4 (xs!!8 ) (xs!!9 ) (xs!!10) (xs!!11) 
    w  = V4 (xs!!12) (xs!!13) (xs!!14) (xs!!15)
    xs = fmap realToFrac xs' :: [Double]

-- | A convenience function to construct @V3 a@ from a list of @[a]@
toV3 :: [a] -> V3 a
toV3 xs = V3 (head xs) (xs!!1) (xs!!2)


-- (<$.>) :: (a -> b) -> [a] -> [b]
-- (<$.>) = fmap

-- | Applicative list zipper:

-- |
-- === __Examples:__
-- > vaoArgs       = (\idx' st' vao' -> (idx', st', vao')) <$.> is_ <*.> st_ <*.> vs_
(<*.>) :: [a -> b] -> [a] -> [b]
(<*.>) = zipWith ($)

-- | Right Shift List
rsl :: Int -> [a] -> [a]
rsl _ [] = []
rsl n xs = zipWith const (drop n (cycle xs)) xs

-- rotateList' :: (Int, [a]) -> [a]
-- rotateList' (_, []) = []
-- rotateList' (n, xs) = zipWith const (drop n (cycle xs)) xs

-- | Convert a UUID into a unique Texture uint ID (to be used in conjunction with a texture binding slot).
fromUUID :: UUID -> GLuint
fromUUID = read . concatMap show . (\ (x,y,z,w)-> fmap toInteger [x,y,z,w]) . toWords

-- | Generates a UUID, based on a String (FilePath)
encodeStringUUID :: String -> UUID
encodeStringUUID x = genSeedUUID . fromInteger . fromBase62 . unpack . hashStringToBase62 6 $ pack x

encodeStringInteger :: String -> Integer
encodeStringInteger x = fromBase62 . unpack . hashStringToBase62 1 $ pack x

genSeedUUID :: Int -> UUID
genSeedUUID seed =
  let
      g0      = mkStdGen seed -- RNG from seed
      (u1, _) = random g0
  in u1

  
