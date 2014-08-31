module Vacivity.Utils where

import Control.Parallel.Strategies

onlyIf :: Bool -> (a -> a) -> a -> a
onlyIf b f = if b then f else id

-- | Parallel fmap.
(<$=>) :: (NFData b) => (a -> b) -> [a] -> [b]
f <$=> ls = (parMap rdeepseq) f ls
