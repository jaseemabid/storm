module Control.Concurrent.Actor where

import Prelude hiding (map)

import Control.Concurrent
import Control.Monad (void)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans (liftIO)
import Data.Map.Lazy as Map (Map, keys, lookup, insert)

type Address = ThreadId

-- [todo] Make data a typeclass
-- http://chrisdone.com/posts/data-typeable
data Data = Data Address Integer

instance Show Data where
    show (Data address string) = show string ++ " from " ++ pp address

data Message = Message Address Data
type Mailbox = Chan Data

-- | Bus is a map from the process to its mailbox in an MVar
type Bus = MVar (Map Address Mailbox)

-- | The actor monad, state monad on top of 'IO'.
type ActorM = ReaderT Bus IO

-- | Actor is a monadic action in the 'ActorM' monad, returning ()
type Actor = ActorM ()

-- | Ask an actor's own address
self :: ActorM Address
self = liftIO myThreadId

-- | Spawns a process.
--
-- Adds an entry to bus with an empty mailbox and returns the address.
spawn :: Actor -> ActorM Address
spawn process = do
    state <- ask
    let action = initialize >>= (\pid -> process >> cleanup pid)
    liftIO $ forkIO $ void $ runReaderT action state

-- | Init that is run after the actor is created in the *new* thread
initialize :: ActorM Address
initialize = do
    state <- ask
    pid <- self
    mbox <- liftIO newChan

    liftIO $ modifyMVar_ state $ \map -> return $ insert pid mbox map
    return pid

-- Cleanup that is run after every thread is shutdown
cleanup :: Address -> Actor
cleanup _add = return ()

bus :: ActorM (Map Address Mailbox)
bus = do
    mvar <- ask
    liftIO $ readMVar mvar

send :: Address -> Data -> Actor
send pid message = do
    map <- bus
    case Map.lookup pid map of
        Just mbox ->
            liftIO $ writeChan mbox message
        Nothing ->
            -- Sending a message to a process that doesn't exist is a no op
            return ()

(!) :: Address -> Data -> Actor
(!) = send

receive :: ActorM Data
receive = do
    pid <- self
    map <- bus
    case Map.lookup pid map of
        Just mbox ->
            liftIO $ readChan mbox
        Nothing ->
            error $ "Process " ++ show pid ++ " is a zombie"

-- Helpers

-- | Ask the bus from state
registered :: ActorM [Address]
registered = do
    map <- bus
    return $ keys map

pp :: Address -> String
pp add = show (read (drop 9 $ show add) :: Int)
