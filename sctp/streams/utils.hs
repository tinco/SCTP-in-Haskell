module SCTP.Streams.Utils where
import SCTP.Streams.Event
import SCTP.Streams.Types

data AccumulatorState a = Accumulated a | Accumulating (Accumulator a) | Skipped
type Accumulator a = Event -> AccumulatorState a

accumulate acc handler state event = case acc event of
                                       Accumulated result -> Just (handler state result)
                                       Accumulating acc' -> Just (state {socketHandlers = socketHandlers state ++ [accumulate acc' handler]} ,[])
                                       Skipped -> Nothing

initAccumulator = accumulate (initAccumulator_ (Nothing, Nothing, Nothing))
initAccumulator_ (Nothing, Nothing, t) (GotRandomInteger i) = initAccumulator' (Just i, Nothing, t)
initAccumulator_ (i, Nothing, t) (GotRandomInteger j) = initAccumulator' (i, Just j, t)
initAccumulator_ (i, j, Nothing) (Time t) = initAccumulator' (i,j,Just t)
initAccumulator_ _ _ = Skipped
initAccumulator'  t@(Just _, Just _, Just _) = Accumulated t
initAccumulator'  t = Accumulating (initAccumulator_ t)

