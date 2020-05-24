module Streaming.Internal where 

data Stream f m r 
  = Step (f (Stream f m r))
  | Effect (m (Stream f m r))
  | Return r
