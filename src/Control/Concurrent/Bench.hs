module Control.Concurrent.Bench where

import Prelude hiding (map)

import Control.Concurrent
import Control.Monad (void)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Trans (liftIO)
import Data.Map.Lazy as Map (empty)
import Data.Time

import Control.Concurrent.Actor

data M = M Address Integer

actor :: MVar () -> Actor M
actor finish = do
    pid <- self
    loop pid 0
  where
    loop :: Address -> Int -> Actor M
    loop _add 100000 = do
        liftIO $ putMVar finish ()
        return ()
    loop pid counter = do
        msg <- receive
        case msg of
            M address i ->
                address ! M pid (i + 1)
        loop pid $ counter + 1

run :: Actor M
run = do
    start <- liftIO getCurrentTime

    one <- liftIO newEmptyMVar
    two <- liftIO newEmptyMVar

    first <- spawn $ actor one
    second <- spawn $ actor two

    second ! M first 0

    liftIO $ takeMVar one
    liftIO $ takeMVar two

    stop <- liftIO getCurrentTime
    liftIO $ print $ diffUTCTime stop start
    return ()

bench :: IO ()
bench = void $ newMVar empty >>= runReaderT run
