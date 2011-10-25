import Debug.Trace

type Event = Bool
type Events = [Event]
type StateA = Char
type StateB = Int

newtype Handler = Handler {runHandler :: Events -> (Events,Handler)}

createHandler :: (s -> Events -> (s, Events)) -> s -> Handler
createHandler f initS = Handler ( \i -> let (s',o) = f initS i
                                        in  (o, createHandler f s')
                                )

useHandlers :: Events -> [Handler] -> (Events,[Handler])
useHandlers e []     = (e,[])
useHandlers e (h:hs) = (e'',(h':hs'))
  where
    (e',h')   = (runHandler h) e
    (e'',hs') = useHandlers e' hs

dealWithIO :: [Handler] -> Events -> Events
dealWithIO handlers []     = []
dealWithIO handlers events = dealWithIO handlers' events'
  where
    (events',handlers') = useHandlers events handlers

aHandler :: Handler
aHandler = createHandler aTransfer 'c'

aTransfer :: StateA -> Events -> (StateA,Events)
aTransfer 'c' (True:es) = ('d',True:es)
aTransfer 'd' (True:es) = ('e',es)
aTransfer s   (True:es) = (s,False:es)
aTransfer s   es        = (s,es)

bHandler :: Handler
bHandler = createHandler bTransfer 0

bTransfer :: StateB -> Events -> (StateB,Events)
bTransfer 0 (True:es)  = (0,True:es)
bTransfer n (False:es) = trace (show n) (n+1,es)
bTransfer n es         = trace (show n) (n,es)
