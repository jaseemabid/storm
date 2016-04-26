module Control.Concurrent.Bench (main) where

import Prelude hiding (map)

import Control.Concurrent
import Control.Monad (void)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Trans (liftIO)
import Criterion.Main (defaultMain, bench, whnf, bgroup)
import Data.Map.Lazy as Map (empty)

import Control.Concurrent.Actor

data M = M Address Integer

increment :: MVar () -> Int -> Actor M
increment finish count = do
    pid <- self
    loop pid 0
  where
    loop :: Address -> Int -> Actor M
    loop _add count = do
        liftIO $ putMVar finish ()
        return ()
    loop pid counter = do
        msg <- receive
        case msg of
            M address i ->
                address ! M pid (i + 1)
        loop pid $ counter + 1

run :: Int -> IO ()
run count = void $ newMVar empty >>= runReaderT run'
  where
    run' :: Actor M
    run' = do
        one <- liftIO newEmptyMVar
        two <- liftIO newEmptyMVar

        first <- spawn $ increment one count
        second <- spawn $ increment two count

        second ! M first 0

        liftIO $ takeMVar one
        liftIO $ takeMVar two

        return ()

main = defaultMain [
    bgroup "run" [ bench "100K"  $ whnf run 100000
                 , bench "500K"  $ whnf run 500000
                 , bench "1M"  $ whnf run 100000
                 ]
    ]
