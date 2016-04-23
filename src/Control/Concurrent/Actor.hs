module Control.Concurrent.Actor where

import Control.Concurrent
import Control.Monad (void)
import Control.Monad.State (StateT, get, put, runStateT)
import Control.Monad.Trans (liftIO)
import Text.Printf (printf)
import Debug.Trace (trace)
import Data.Map.Lazy as Map

debug :: a -> String -> a
debug = flip trace

type Address = ThreadId

-- [todo] Make data a typeclass
-- http://chrisdone.com/posts/data-typeable
data Data = S String
          | A Address
          | C Address String  -- Compound

instance Show Data where
    show (S string) = show string
    show (A address) = pp address
    show (C address string) = show string ++ " from " ++ pp address

data Message = Message Address Data
type Mailbox = Chan Data

-- | Bus is a map from the process to its mailbox
type Bus = Map Address Mailbox

-- | The actor monad, state monad on top of 'IO'.
type ActorM = StateT Bus IO

-- | Actor is a monadic action in the 'ActorM' monad, returning ()
type Actor = ActorM ()

-- | Get an actor's own address
self :: ActorM Address
self = liftIO myThreadId

-- | Spawns a process.
--
-- Adds an entry to bus with an empty mailbox and returns the address.
spawn :: Actor -> ActorM Address
spawn process = do
    bus <- get
    -- Make a mailbox that is shared b/w parent and child thread
    mbox <- liftIO (newChan :: IO Mailbox)

    let action = initialize mbox >>= (\pid -> process >> cleanup pid)

    -- [review] - How do I run stateful operation inside IO monad?
    pid <- liftIO $ forkIO $ void $ runStateT action bus

    -- Add mbox to state of parent thread
    put $ insert pid mbox bus
    return pid

-- | Init that is run after the actor is created in the *new* thread
initialize :: Mailbox -> ActorM Address
initialize mbox = do
    bus <- get
    pid <- self
    -- Add mbox to state of the new child thread
    put $ insert pid mbox bus
    return pid

-- Cleanup that is run after every thread is shutdown
cleanup :: Address -> Actor
cleanup _add = return ()

send :: Address -> Data -> Actor
send pid message = do
    me <- self
    bus <- get
    case Map.lookup pid bus of
        Just mbox ->
            liftIO $ writeChan mbox message
        Nothing ->
            -- Sending a message to a process that doesn't exist is a no op
            -- error $ "Process " ++ show pid ++ " is a zombie"
            trace ("❌ " ++ pp me ++ " → " ++ pp pid) return ()

receive :: ActorM Data
receive = do
    pid <- self
    bus <- get
    case Map.lookup pid bus of
        Just mbox ->
            liftIO $ readChan mbox
        Nothing ->
            error $ "Process " ++ show pid ++ " is a zombie"

actor :: Int -> Actor
actor wait = do
    liftIO $ threadDelay wait
    pid <- self
    liftIO $ printf "Hello I'm %v\n" (pp pid)

    loop
  where
    loop :: Actor
    loop = do
        pid <- self
        msg <- receive
        case msg of
            S str ->
                liftIO $ printf "%s got message : %s\n" (pp pid) (show str)

            C add "PING" -> do
                liftIO $ printf "%s PING\n" (pp pid)
                send add $ C pid "PONG"

            C add "PONG" -> do
                liftIO $ printf "%s PONG\n" (pp pid)
                send add $ C pid "PING"
        loop

run :: Actor
run = do
    first <- spawn $ actor 00
    second <- spawn $ actor 1000

    -- send first $ C second "PING"
    send second $ C first "PING"

    liftIO $ threadDelay 5000000
    return ()

main :: IO ()
main = void $ runStateT run (empty :: Bus)

-- Helpers

-- | Get the bus from state
registered :: ActorM [Address]
registered = do
    bus <- get
    return $ keys bus

pp :: Address -> String
pp add = show (read (drop 9 $ show add) :: Int)
