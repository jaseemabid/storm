module Control.Concurrent.Bench where

import Prelude hiding (map)

import Control.Concurrent
import Control.Monad (void)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Trans (liftIO)
import Data.Map.Lazy as Map (empty)
import Data.Time

import Control.Concurrent.Actor

actor :: MVar () -> Actor
actor finish = do
    pid <- self
    loop pid 0
  where
    loop :: Address -> Int -> Actor
    loop _add 100000 = do
        liftIO $ putMVar finish ()
        return ()
    loop pid counter = do
        msg <- receive
        case msg of
            Data address i ->
                address ! Data pid (i + 1)
        loop pid $ counter + 1

run :: Actor
run = do
    start <- liftIO getCurrentTime

    one <- liftIO newEmptyMVar
    two <- liftIO newEmptyMVar

    first <- spawn $ actor one
    second <- spawn $ actor two

    second ! Data first 0

    liftIO $ takeMVar one
    liftIO $ takeMVar two

    stop <- liftIO getCurrentTime
    liftIO $ print $ diffUTCTime stop start
    return ()

bench :: IO ()
bench = void $ newMVar empty >>= runReaderT run
