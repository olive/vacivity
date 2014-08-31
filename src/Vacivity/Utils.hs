module Vacivity.Utils where


onlyIf :: Bool -> (a -> a) -> a -> a
onlyIf b f = if b then f else id
