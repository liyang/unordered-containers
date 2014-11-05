-- for n in 2 4 8 16 32 64 128 256 512 1024 2048 4096 ; do SIZE=$n dist/build/benchmarks/benchmarks -o ~/Shared/unordered/$n.html ; done
{-# LANGUAGE CPP, GADTs, PackageImports #-}

module Main where

import Control.Applicative
import Control.DeepSeq
import Control.Exception (evaluate)
import Control.Monad.Trans (liftIO)
import Criterion.Config
import Criterion.Main
import Data.Bits ((.&.))
import Data.Hashable (Hashable)
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Prelude hiding (lookup)
import System.Environment
import Text.Read

import qualified Util.Int as UI

#if !MIN_VERSION_bytestring(0,10,0)
instance NFData BS.ByteString
#endif

data B where
    B :: NFData a => a -> B

instance NFData B where
    rnf (B b) = rnf b

main :: IO ()
main = do
    n <- maybe 4096 id . (=<<) readMaybe <$> lookupEnv "SIZE"

    let keysI   = UI.rnd (n+n) n
    let elemsI  = zip keysI [1..n]

    let keysI'  = UI.rnd' (n+n) n
    let elemsI2 = zip [n `div` 2..n + (n `div` 2)] [1..n]  -- for union

    let keysDupI   = UI.rnd (n`div`4) n
    let elemsDupI  = zip keysDupI [1..n]

    let mi      = M.fromList elemsI :: M.Map Int Int
    let mi2     = M.fromList elemsI2 :: M.Map Int Int
    let im      = IM.fromList elemsI :: IM.IntMap Int
    let im2     = IM.fromList elemsI2 :: IM.IntMap Int
    let hmi     = HM.fromList elemsI :: HM.HashMap Int Int
    let hmi2    = HM.fromList elemsI2 :: HM.HashMap Int Int
    defaultMainWith defaultConfig
        (liftIO . evaluate $ rnf [B mi, B mi2, B im, B im2, B hmi, B hmi2])

          -- ** Map
        [ bgroup "Map"
          [ bench "lookup" $ whnf (lookupM keysI) mi
          , bench "lookup-miss" $ whnf (lookupM keysI') mi
          , bench "insert" $ whnf (insertM elemsI) M.empty
          , bench "insert-dup" $ whnf (insertM elemsI) mi
          , bench "delete" $ whnf (deleteM keysI) mi
          , bench "delete-miss" $ whnf (deleteM keysI') mi
          , bench "union" $ whnf (M.union mi) mi2
          , bench "map" $ whnf (M.map (\ v -> v + 1)) mi
          , bench "difference" $ whnf (M.difference mi) mi2
          , bench "intersection" $ whnf (M.intersection mi) mi2
          , bench "foldl'" $ whnf (M.foldl' (+) 0) mi
          , bench "foldr" $ nf (M.foldr (:) []) mi
          , bench "filter" $ whnf (M.filter (\ v -> v .&. 1 == 0)) mi
          , bench "filterWithKey" $ whnf (M.filterWithKey (\ k _ -> k .&. 1 == 0)) mi
          , bench "size" $ whnf M.size mi
          , bench "fromList" $ whnf M.fromList elemsI
          , bench "fromList-dup" $ whnf M.fromList elemsDupI
          ]

          -- ** IntMap
        , bgroup "IntMap"
          [ bench "lookup" $ whnf (lookupIM keysI) im
          , bench "lookup-miss" $ whnf (lookupIM keysI') im
          , bench "insert" $ whnf (insertIM elemsI) IM.empty
          , bench "insert-dup" $ whnf (insertIM elemsI) im
          , bench "delete" $ whnf (deleteIM keysI) im
          , bench "delete-miss" $ whnf (deleteIM keysI') im
          , bench "union" $ whnf (IM.union im) im2
          , bench "map" $ whnf (IM.map (\ v -> v + 1)) im
          , bench "difference" $ whnf (IM.difference im) im2
          , bench "intersection" $ whnf (IM.intersection im) im2
          , bench "foldl'" $ whnf (IM.foldl' (+) 0) im
          , bench "foldr" $ nf (IM.foldr (:) []) im
          , bench "filter" $ whnf (IM.filter (\ v -> v .&. 1 == 0)) im
          , bench "filterWithKey" $ whnf (IM.filterWithKey (\ k _ -> k .&. 1 == 0)) im
          , bench "size" $ whnf IM.size im
          , bench "fromList" $ whnf IM.fromList elemsI
          , bench "fromList-dup" $ whnf IM.fromList elemsDupI
          ]

        , bgroup "HashMap"
          [ -- * Basic interface
            bench "lookup" $ whnf (lookup keysI) hmi
          , bench "lookup-miss" $ whnf (lookup keysI') hmi
          , bench "insert" $ whnf (insert elemsI) HM.empty
          , bench "insert-dup" $ whnf (insert elemsI) hmi
          , bench "delete" $ whnf (delete keysI) hmi
          , bench "delete-miss" $ whnf (delete keysI') hmi
          , bench "union" $ whnf (HM.union hmi) hmi2
          , bench "map" $ whnf (HM.map (\ v -> v + 1)) hmi
          , bench "difference" $ whnf (HM.difference hmi) hmi2
          , bench "intersection" $ whnf (HM.intersection hmi) hmi2
          , bench "foldl'" $ whnf (HM.foldl' (+) 0) hmi
          , bench "foldr" $ nf (HM.foldr (:) []) hmi
          , bench "filter" $ whnf (HM.filter (\ v -> v .&. 1 == 0)) hmi
          , bench "filterWithKey" $ whnf (HM.filterWithKey (\ k _ -> k .&. 1 == 0)) hmi
          , bench "size" $ whnf HM.size hmi
          , bench "fromList" $ whnf HM.fromList elemsI
          , bench "fromList-dup" $ whnf HM.fromList elemsDupI
          ]

        ]

------------------------------------------------------------------------
-- * HashMap

lookup :: (Eq k, Hashable k) => [k] -> HM.HashMap k Int -> Int
lookup xs m = foldl' (\z k -> fromMaybe z (HM.lookup k m)) 0 xs
{-# SPECIALIZE lookup :: [Int] -> HM.HashMap Int Int -> Int #-}
{-# SPECIALIZE lookup :: [String] -> HM.HashMap String Int -> Int #-}
{-# SPECIALIZE lookup :: [BS.ByteString] -> HM.HashMap BS.ByteString Int
                      -> Int #-}

insert :: (Eq k, Hashable k) => [(k, Int)] -> HM.HashMap k Int
       -> HM.HashMap k Int
insert xs m0 = foldl' (\m (k, v) -> HM.insert k v m) m0 xs
{-# SPECIALIZE insert :: [(Int, Int)] -> HM.HashMap Int Int
                      -> HM.HashMap Int Int #-}
{-# SPECIALIZE insert :: [(String, Int)] -> HM.HashMap String Int
                      -> HM.HashMap String Int #-}
{-# SPECIALIZE insert :: [(BS.ByteString, Int)] -> HM.HashMap BS.ByteString Int
                      -> HM.HashMap BS.ByteString Int #-}

delete :: (Eq k, Hashable k) => [k] -> HM.HashMap k Int -> HM.HashMap k Int
delete xs m0 = foldl' (\m k -> HM.delete k m) m0 xs
{-# SPECIALIZE delete :: [Int] -> HM.HashMap Int Int -> HM.HashMap Int Int #-}
{-# SPECIALIZE delete :: [String] -> HM.HashMap String Int
                      -> HM.HashMap String Int #-}
{-# SPECIALIZE delete :: [BS.ByteString] -> HM.HashMap BS.ByteString Int
                      -> HM.HashMap BS.ByteString Int #-}

------------------------------------------------------------------------
-- * Map

lookupM :: Ord k => [k] -> M.Map k Int -> Int
lookupM xs m = foldl' (\z k -> fromMaybe z (M.lookup k m)) 0 xs
{-# SPECIALIZE lookupM :: [String] -> M.Map String Int -> Int #-}
{-# SPECIALIZE lookupM :: [BS.ByteString] -> M.Map BS.ByteString Int -> Int #-}

insertM :: Ord k => [(k, Int)] -> M.Map k Int -> M.Map k Int
insertM xs m0 = foldl' (\m (k, v) -> M.insert k v m) m0 xs
{-# SPECIALIZE insertM :: [(String, Int)] -> M.Map String Int
                       -> M.Map String Int #-}
{-# SPECIALIZE insertM :: [(BS.ByteString, Int)] -> M.Map BS.ByteString Int
                       -> M.Map BS.ByteString Int #-}

deleteM :: Ord k => [k] -> M.Map k Int -> M.Map k Int
deleteM xs m0 = foldl' (\m k -> M.delete k m) m0 xs
{-# SPECIALIZE deleteM :: [String] -> M.Map String Int -> M.Map String Int #-}
{-# SPECIALIZE deleteM :: [BS.ByteString] -> M.Map BS.ByteString Int
                       -> M.Map BS.ByteString Int #-}

------------------------------------------------------------------------
-- * IntMap

lookupIM :: [Int] -> IM.IntMap Int -> Int
lookupIM xs m = foldl' (\z k -> fromMaybe z (IM.lookup k m)) 0 xs

insertIM :: [(Int, Int)] -> IM.IntMap Int -> IM.IntMap Int
insertIM xs m0 = foldl' (\m (k, v) -> IM.insert k v m) m0 xs

deleteIM :: [Int] -> IM.IntMap Int -> IM.IntMap Int
deleteIM xs m0 = foldl' (\m k -> IM.delete k m) m0 xs
