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
actor finish = loop 0
  where
    loop :: Int -> Actor
    loop 100000 = do
        liftIO (putMVar finish ())
        return ()
    loop counter = do
        pid <- self
        msg <- receive
        case msg of
            Data add i -> send add $ Data pid (i + 1)
        loop $ counter + 1

run :: Actor
run = do
    start <- liftIO getCurrentTime

    one <- liftIO newEmptyMVar
    two <- liftIO newEmptyMVar

    first <- spawn $ actor one
    second <- spawn $ actor two

    send second $ Data first 0

    liftIO $ takeMVar one
    liftIO $ takeMVar two

    stop <- liftIO getCurrentTime
    liftIO $ print $ diffUTCTime stop start
    return ()

bench :: IO ()
bench = void $ newMVar empty >>= runReaderT run
