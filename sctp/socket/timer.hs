module SCTP.Socket.Timer where
import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar
import System.CPUTime

type Timer k = MVar ([(k,TimerEntry)],Bool)

data TimerEntry = TimerEntry {
  timerTime :: Integer,
  timerCallback :: IO()
}

-- startTimer runs a loop in which it checks its timers list every 'interval' milliseconds
startTimer :: Int -> IO(Timer k)
startTimer interval = do
    mvar <- newMVar ([],True)
    let timerLoop = do
        (timers,running) <- takeMVar mvar
        time' <- getCPUTime
        let time = toMilliseconds time'
        let checkTime timer entry@(_, e) = do
            if (time > timerTime e)
              then do
                  forkIO $ timerCallback e
                  return timer
              else do
                  return $ entry : timer
        timers' <- foldM checkTime [] timers
        putMVar mvar (timers',running)
        threadDelay $ interval * 1000
        when running timerLoop
    forkIO timerLoop
    return mvar

toMilliseconds time = time `div` (10^9)

-- stops the timer
stopTimer :: Timer k -> IO()
stopTimer timerVar = do
    (timers,_) <- takeMVar timerVar
    putMVar timerVar (timers,False)
    
            
-- registerTimer adds a callback to the timers list, the callback will be called after dt milliseconds
registerTimer :: Timer k -> k -> IO() -> Integer -> IO()
registerTimer timerVar id callback dt = do
    (timers,r) <- takeMVar timerVar
    time' <- getCPUTime
    let time = toMilliseconds time'
    let entry = TimerEntry (time+dt) callback
    let timers' = (id,entry) : timers
    putMVar timerVar (timers',r)

-- execution of the given timer will be cancelled
cancelTimer :: Eq k => Timer k -> k -> IO()
cancelTimer timerVar id = do
    (timers,r) <- takeMVar timerVar
    let timers' = filter (\ (id',_) -> id /= id') timers
    putMVar timerVar (timers',r)

{- What we want to do is:
  - Send a packet and see if a response is received in time
  - RTO is in milliseconds
  - if it is then we want to:
  -    ACK it. removing it from a queue of to-be-acknowledged packets
  -    measure the round trip  time
  -}
 
