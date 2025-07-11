--------------------------------------------------------------------------------
-- |
-- Module      :  Utils
-- Copyright   :  (c) Vladimir Lopatin 2024
-- License     :  BSD3
--
-- Maintainer  :  Vladimir Lopatin <madjestic13@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- Utilities and helper functions for varios aspects of graphics pipeline.
--
--------------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE TypeApplications #-}

module Graphics.RedViz.Utils
  ( toIdxVAO
  , toIdxVAO'
  , Graphics.RedViz.Utils.fromList
  , (<$.>)
  , (<*.>)
  , (^*^)
  , toV3
  , toV3'
  , toV4
  , rotateList
  , rotateList'
  , fromUUID
  , encodeStringUUID
  , vectorizedCompose
  , hashUUID
  , intToWord32
  , word32ToInt
  , wordToFloat
  , floatToWord32
  , wordToDouble
  , doubleToWord32
  , intToWord8
  , double2M44GLfloat
  , double2V3Float
  ) where

import GHC.ST (runST, ST)
import Lens.Micro.Extras
import Graphics.Rendering.OpenGL as GL (GLfloat)
import Data.ByteString.Char8           (pack
                                       ,unpack)
import Foreign.C.Types
import Data.Binary
import Data.Set                  as DS (fromList, toList)
import Data.List.Index                 (indexed)
import Data.List                       (elemIndex)
import Data.List                 as DL (transpose)
import Data.Locator
import Data.UUID                 as U
import Data.Vector               as DV (fromList, (!), map, toList, slice)
import Data.VectorSpace          as DV
import Data.Maybe (fromJust)
import Graphics.Rendering.OpenGL (GLuint)
import Linear.V                        (fromVector, fromV)
import Linear.V3 
import Linear.V4
import Linear.Matrix
import Linear.Metric             as LM
import System.Random

import Data.Binary --(decode,encode,Binary)
import Crypto.Hash.SHA256 (hashlazy)
import Data.UUID.Types
import GHC.Generics
import Data.ByteString.Lazy (fromStrict)

import Data.Array.ST (newArray, readArray, MArray, STUArray)
import Data.Array.Base (castSTUArray)

import Data.Hashable
-- import Debug.Trace as DT

-- instance Binary CInt where
--   put x = do put x
--   get = do x <- get
--            return x

instance VectorSpace (V3 Double) Double where
  zeroVector                   = (V3 0 0 0)
  (*^) s (V3 x y z)            = (V3 (s*x) (s*y) (s*z))
  (^+^)  (V3 x y z) (V3 k l m) = (V3 (x+k) (y+l) (z+m))
  dot    (V3 x y z) (V3 k l m) = (x*k) + (y*l) + (z*m)

instance VectorSpace (V4 (V4 Double)) Double where
  zeroVector                   = identity :: M44 Double
  (*^) s (m :: M44 Double)     = m !!* s
  (^+^)  (m :: M44 Double) (n :: M44 Double) = 
    mkTransformationMat
    rot
    tr
     where
      rot = LM.normalize $ (inv33 m') !*! (n')
        where
          m' = view _m33 m
          n' = view _m33 n
      tr = (view translation m) ^+^ (view translation n)
  dot    (m :: M44 Double) (n :: M44 Double) = DV.dot m n

double2M44GLfloat :: V4 (V4 Double) -> M44 GLfloat
double2M44GLfloat = fmap . fmap $ realToFrac

double2V3Float :: V3 Double -> V3 Float
double2V3Float = fmap realToFrac

vectorizedCompose :: [[M44 Double]] -> [M44 Double]
vectorizedCompose = fmap (foldr1 (^*^)) . DL.transpose

(^*^) :: M44 Double -> M44 Double -> M44 Double
(^*^) mtx0 mtx1 = mkTransformationMat rot tr
  where
    rot = view _m33 mtx0 !*! view _m33 mtx1 :: M33 Double
    --rot = LM.identity :: M33 Double -- DEBUG
    tr  = view translation mtx0 ^+^ view translation mtx1

-- | [Float]  ~= vertex
--  [[Float]] ~= VAO
toIdxVAO :: [[Float]] -> ([Int],[Float])
toIdxVAO vao = (idx, idxVAO)
  where
    iListSet = indexed $ DS.toList $ DS.fromList $ vao                       :: [(Int,[Float])]
    iList    = indexed vao                                                   :: [(Int, [GLfloat])]
    idx      = fmap fst (matchLists iListSet iList) :: [Int]
    idxVAO   = concat $ fmap (\x -> snd x) iListSet                          :: [Float]

toIdxVAO' :: [[Float]] -> ([Int],[Float])
toIdxVAO' vao = (idx, idxVAO)
  where
    --iListSet = indexed $ DS.toList $ DS.fromList $ vao                       :: [(Int,[Float])]
    iList    = indexed vao                             :: [(Int, [GLfloat])]
    idx      = fmap fst iList :: [Int]
    idxVAO   = concat $ fmap (\x -> snd x) iList       :: [Float]

-- | matchLists - cross-match 2 listst, replacing the elements of list2 with matching
-- |          with elements of list1, concatenating the non-matched elements.
-- |   il - indexed list
-- |  nil - non-indexed list
matchLists :: [(Int, [GLfloat])] -> [(Int, [GLfloat])] -> [(Int, [GLfloat])]
matchLists il nil' =
  fmap (mFunc il) nil' -- mFunc - matching function
  where
    -- | il      - indexed list
    -- | nile    - non indexed list element
    -- | Replaces the target element with the first match from the matching list il
    il' = DV.fromList il
    cxs'  = DV.map snd il'
    mFunc _ (iy, cy) =
      (\case
          Just idx -> il' ! idx
          Nothing  -> (-iy, cy) ) nili      -- if a unique index idx found - flip the sign
                                            -- the idea idx to separate normal indexes
                                            -- and unique indexes -> [idx:uidx] later
      where
        nili = elemIndex cy (DV.toList cxs')
        -- cxs  = DV.map (\(i,s) -> s) il' -- :: [[GLfloat]]

-- TODO: create a fromList typeclass?
-- [a] -> V3 a
-- [a] -> M44 a
-- etc.
fromList :: Maybe [Float] -> M44 Double
fromList xs0 = m44
  where
    m44 = 
      case xs0 of
        Just xs' -> V4 x y z w
          where
            x  = V4 (head xs) (xs!!1 ) (xs!!2 ) (xs!!3 ) 
            y  = V4 (xs!!4 )  (xs!!5 ) (xs!!6 ) (xs!!7 ) 
            z  = V4 (xs!!8 )  (xs!!9 ) (xs!!10) (xs!!11) 
            w  = V4 (xs!!12)  (xs!!13) (xs!!14) (xs!!15)
            xs = fmap realToFrac xs' :: [Double]
        Nothing -> identity

(<$.>) :: (a -> b) -> [a] -> [b]
(<$.>) = fmap

(<*.>) :: [a -> b] -> [a] -> [b]
(<*.>) = zipWith ($)

toV3 :: [a] -> V3 a
toV3 xs = case length xs of
  3 -> fromV . fromJust . fromVector $ DV.fromList xs
  _ -> error "the list artument to toV3 is length != 3"

-- like toV3 but with slice support
toV3' :: Int -> Int -> [a] -> V3 a
toV3' k l xs = fromV . fromJust . fromVector $ slice k l $ DV.fromList xs

toV4 :: [a] -> V4 a
--toV4 xs = V4 (head xs) (xs!!1) (xs!!2) (xs!!3)
toV4 xs = fromV . fromJust . fromVector $ DV.fromList xs

rotateList :: Int -> [a] -> [a]
rotateList _ [] = []
rotateList n xs = zipWith const (drop n (cycle xs)) xs

rotateList' :: (Int, [a]) -> [a]
rotateList' (_, []) = []
rotateList' (n, xs) = zipWith const (drop n (cycle xs)) xs

fromUUID :: UUID -> GLuint
fromUUID = read . concatMap show . (\ (x,y,z,w)-> fmap toInteger [x,y,z,w]) . toWords

-- | Generate a UUID, based on FilePath
-- | e.g. fromUUID $ encodeStringUUID "./projects/.temp1"
-- | > 2836415114
encodeStringUUID :: String -> UUID
encodeStringUUID x = genSeedUUID . fromInteger . fromBase62 . unpack . hashStringToBase62 6 $ pack x

-- encodeStringInteger :: String -> Integer
-- encodeStringInteger x = fromBase62 . unpack . hashStringToBase62 1 $ pack x

genSeedUUID :: Int -> UUID
genSeedUUID seed =
  let
      g0      = mkStdGen seed -- RNG from seed
      (u1, _) = random g0
  in u1

data Foo = Foo Int String
    deriving (Eq,Ord,Show,Generic,Binary)

--hash' :: Hashable a => a -> 

-- hashUUID :: IO ()
-- hashUUID = print (decode @UUID (fromStrict (hashlazy (encode (Foo 1 "hello")))))

hashUUID :: Hashable a => a -> UUID
hashUUID a = (decode @UUID (fromStrict (hashlazy (encode $ hash a))))
  
word32ToInt :: Word32 -> Int
word32ToInt x = runST (cast x)

intToWord32 :: Int -> Word32
intToWord32 x = runST (cast x)

wordToFloat :: Word32 -> Float
wordToFloat x = runST (cast x)

floatToWord32 :: Float -> Word32
floatToWord32 x = runST (cast x)

wordToDouble :: Word32 -> Double
wordToDouble x = runST (cast x)

doubleToWord32 :: Double -> Word32
doubleToWord32 x = runST (cast x)

intToWord8 :: Int -> Word8
intToWord8 x = runST (cast x)

{-# INLINE cast #-}
cast :: (MArray (STUArray s) a (ST s),
         MArray (STUArray s) b (ST s)) => a -> ST s b
cast x = newArray (0 :: Int, 0) x >>= castSTUArray >>= flip readArray 0

  
