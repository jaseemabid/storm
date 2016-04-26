module Control.Concurrent.Actor where

import Prelude hiding (map)

import Control.Concurrent
import Control.Monad (void)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans (liftIO)
import Data.Map.Lazy as Map (Map, keys, lookup, insert)

type Address = ThreadId

data Message a = Message Address a
type Mailbox a = Chan a

-- | Bus is a map from the process to its mailbox in an MVar
type Bus a = MVar (Map Address (Mailbox a))

-- | The actor monad, state monad on top of 'IO'.
type ActorM a = ReaderT (Bus a) IO

-- | Actor is a monadic action in the 'ActorM' monad, returning ()
type Actor a = ActorM a ()

-- | Ask an actor's own address
self :: ActorM a Address
self = liftIO myThreadId

-- | Spawns a process.
--
-- Adds an entry to bus with an empty mailbox and returns the address.
--
-- Messages sent b/w the small interval after spawn returns and before the
-- mailbox is setup will be lead to message loss. await mvar fixes that.
spawn :: Actor a -> ActorM a Address
spawn process = do
    state <- ask
    await <- liftIO newEmptyMVar
    let action = initialize await >>= (\pid -> process >> cleanup pid)
    pid <- liftIO $ forkIO $ void $ runReaderT action state
    liftIO $ takeMVar await
    return pid

-- | Init that is run after the actor is created in the *new* thread
initialize :: MVar () -> ActorM a Address
initialize ready = do
    state <- ask
    pid <- self
    mbox <- liftIO newChan

    liftIO $ modifyMVar_ state $ \map -> return $ insert pid mbox map
    liftIO $ putMVar ready ()
    return pid

-- Cleanup that is run after every thread is shutdown
cleanup :: Address -> Actor a
cleanup _add = return ()

bus :: ActorM a (Map Address (Mailbox a))
bus = do
    mvar <- ask
    liftIO $ readMVar mvar

send :: Address -> a -> Actor a
send pid message = do
    map <- bus
    case Map.lookup pid map of
        Just mbox ->
            liftIO $ writeChan mbox message
        Nothing ->
            -- Sending a message to a process that doesn't exist is a no op
            return ()

(!) :: Address -> a -> Actor a
(!) = send

receive :: ActorM a a
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
registered :: ActorM a [Address]
registered = do
    map <- bus
    return $ keys map

pp :: Address -> String
pp add = show (read (drop 9 $ show add) :: Int)
