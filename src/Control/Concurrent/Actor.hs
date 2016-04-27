{-# LANGUAGE ScopedTypeVariables #-}
module Control.Concurrent.Actor where

import Prelude hiding (map)

import Control.Concurrent
import Control.Monad (void)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans (liftIO)
import Data.Map.Lazy as Map (Map, keys, lookup, insert, fromList)

type Address = ThreadId
type Mailbox a = Chan a

-- | Bus is a map from the process to its mailbox in an MVar
type Bus a = Map Address (Mailbox a)

-- | Registry is a map from name to process ID
-- [todo] - Replace string with a
type Registry = Map String Address

-- | [TODO] - Anything that can be compared must be a Name
type Name = String

-- | GADTs might help to replace Name with `Eq a => ...`
data Context a = Context (Bus a) Registry

-- | The actor monad, state monad on top of 'IO'.
type ActorM a = ReaderT (MVar (Context a)) IO

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
    state :: MVar (Context a) <- ask
    await <- liftIO newEmptyMVar
    let action = initialize await >>= (\pid -> process >> cleanup pid)
    pid <- liftIO $ forkIO $ void $ runReaderT action state
    liftIO $ takeMVar await
    return pid

-- | Init that is run after the actor is created in the *new* thread
initialize :: MVar () -> ActorM a Address
initialize ready = do
    state :: MVar (Context a) <- ask
    pid <- self
    mbox <- liftIO newChan
    -- modifyMVar_ :: MVar a -> (a -> IO a) -> IO ()
    liftIO $ modifyMVar_ state $ update pid mbox
    liftIO $ putMVar ready ()
    return pid
  where
    update :: Address -> Mailbox a -> Context a -> IO (Context a)
    update pid mbox (Context bus registry) =
        return $ Context (insert pid mbox bus) registry

-- Cleanup that is run after every thread is shutdown
cleanup :: Address -> Actor a
cleanup _add = return ()

-- | Fetch exchange from context
exchange :: ActorM a (Map Address (Mailbox a))
exchange = do
    Context bus _registry <- ask >>= liftIO . readMVar
    return bus

-- | Fetch registry from context
registry :: ActorM a Registry
registry = do
    Context _bus registry <- ask >>= liftIO . readMVar
    return registry


register :: String -> Address -> Actor a
register name address = do
    state :: MVar (Context a) <- ask
    liftIO $ modifyMVar_ state update
  where
    update :: Context a -> IO (Context a)
    update (Context bus registry) =
        return $ Context bus $ insert name address registry

-- | Ask the bus from state
registered :: ActorM a [Address]
registered = do
    map <- exchange
    return $ keys map

send :: Address -> a -> Actor a
send pid message = do
    map <- exchange
    case Map.lookup pid map of
        Just mbox ->
            liftIO $ writeChan mbox message
        Nothing ->
            -- Sending a message to a process that doesn't exist is a no op
            return ()

(!) :: Address -> a -> Actor a
(!) = send

-- Send by name
-- [TODO] - Merge sendN with with send if possible
sendN :: Name -> a -> Actor a
sendN name message = do
    map <- registry
    case Map.lookup name map of
        Just pid ->
            send pid message
        Nothing ->
            -- Sending a message to a process that doesn't exist is a no op
            return ()

(!$) :: Name -> a -> Actor a
(!$) = sendN

receive :: ActorM a a
receive = do
    pid <- self
    map <- exchange
    case Map.lookup pid map of
        Just mbox ->
            liftIO $ readChan mbox
        Nothing ->
            error $ "Process " ++ show pid ++ " is a zombie"

-- Helpers

pp :: Address -> String
pp add = show (read (drop 9 $ show add) :: Int)

-- | Default context
context :: Context a
context = Context (fromList []) (fromList [])
