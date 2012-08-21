{-# LANGUAGE MultiParamTypeClasses,FunctionalDependencies,TypeSynonymInstances,FlexibleInstances #-}
module StreamIO where
import Control.Monad
import Control.Concurrent
import Control.Concurrent.Chan

type Handler a e s =  (s -> e -> (s, [a]))
type Eventer e = e -> IO()

instance Eq (Handler a e s)where
  (==) a b = True

instance Eq (Eventer e) where
  (==) a b = True

handlerLoop :: (Action a e, Event e) => s -> Handler a e s -> IO (Eventer e)
handlerLoop startState handler = do
    events <- newChan
    forkIO $ handlerLoop' handler events startState startEvent
    return $ writeChan events

handlerLoop' :: (Action a e, Event e) => Handler a e s -> Chan e -> s -> e -> IO ()
handlerLoop' handler events s event = do
    let (s', actions) = handler s event
    mapM_ (\a -> forkIO $ handleIO (writeChan events) a) actions
    let stop = stopAction `elem` actions
    unless stop $ handlerLoop' handler events s' =<< readChan events
    return ()

class Event e where
    startEvent :: e

class Eq a => Action a e | a -> e  where
    stopAction :: a
    handleIO :: (e -> IO()) -> a -> IO()
