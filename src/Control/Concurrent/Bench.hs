module Control.Concurrent.Bench (main) where

import Prelude hiding (map)

import Control.Concurrent
import Control.Monad (void)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Trans (liftIO)
import Criterion.Main (defaultMain, bench, whnf, bgroup)

import Control.Concurrent.Actor

data M = M Address Integer

bang :: Int -> IO ()
bang count = void $ newMVar context >>= runReaderT run'
  where
    run' :: Actor M
    run' = do
        one <- liftIO newEmptyMVar
        two <- liftIO newEmptyMVar

        first <- spawn $ actor one
        second <- spawn $ actor two

        second ! M first 0

        liftIO $ finish one two

    actor :: MVar () -> Actor M
    actor ready = do
        pid <- self
        loop pid 0
      where
        loop :: Address -> Int -> Actor M
        loop pid counter =
            if counter == count
            then liftIO $ putMVar ready ()
            else do
                M address i <- receive
                address ! M pid (i + 1)
                loop pid $ counter + 1

named :: Int -> IO ()
named count = void $ newMVar context >>= runReaderT run'
  where
    run' :: Actor Int
    run' = do
        one <- liftIO newEmptyMVar
        two <- liftIO newEmptyMVar

        first <- spawn $ actor one "second"
        second <- spawn $ actor two "first"

        register "first" first
        register "second" second

        "first" !$ 0

        liftIO $ finish one two

    actor :: MVar () -> Name -> Actor Int
    actor ready address = loop 0
      where
        loop :: Int -> Actor Int
        loop counter =
            if counter == count
            then liftIO $ putMVar ready ()
            else do
                i <- receive
                address !$ (i + 1)
                loop $ counter + 1

finish :: MVar a -> MVar a1 -> IO ()
finish one two = takeMVar one >> takeMVar two >> return ()

main :: IO ()
main = defaultMain [
    bgroup "run" [
          bench "bang/100K"  $ whnf bang 100000,
          bench "bang/500K"  $ whnf bang 500000,
          bench "bang/1M"  $ whnf bang 100000,

          bench "named/100K"  $ whnf named 100000,
          bench "named/500K"  $ whnf named 500000,
          bench "named/1M"  $ whnf named 100000
          ]
    ]
