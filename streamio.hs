{-# LANGUAGE MultiParamTypeClasses,FunctionalDependencies #-}
module StreamIO where
import Control.Monad
import Control.Concurrent
import Control.Concurrent.Chan

type Handler a e s =  (s -> e -> (s, [a]))
handlerLoop :: (Action a e, Event e, State s) => Handler a e s -> IO()
handlerLoop handler = do
    events <- newChan
    eventLoop handler events startState startEvent

eventLoop :: (Action a e, Event e, State s) => Handler a e s -> Chan e -> s -> e -> IO()
eventLoop handler events s event = do
    let (s', actions) = handler s event
    mapM_ (\a -> forkIO $ handleIO (writeChan events) a) actions
    let stop = stopAction `elem` actions
    unless stop $ eventLoop handler events s' =<< readChan events
    return ()

class Event e where
    startEvent :: e

class Eq a => Action a e | a -> e  where
    stopAction :: a
    handleIO :: a -> IO e

class State s where
    startState :: s
