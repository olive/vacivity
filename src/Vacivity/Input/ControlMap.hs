module Vacivity.Input.ControlMap where

import Control.Applicative

import Antiqua.Input.Controls as C

data ControlKey = CK'Up
                | CK'Down
                | CK'Left
                | CK'Right

    deriving (Eq, Ord)

data ControlMap a = ControlMap (a, a, a, a)

data Index (i :: ControlKey) = Get

sequenceCtrl :: Monad m => ControlMap (m a) -> m (ControlMap a)
sequenceCtrl (ControlMap (x1, x2, x3, x4)) = do
    x1' <- x1
    x2' <- x2
    x3' <- x3
    x4' <- x4
    return $ ControlMap (x1', x2', x3', x4')

class Indexed a i b | a i -> b where
    from :: a -> Index i -> b

instance Indexed (ControlMap a) CK'Up a where
    from (ControlMap (x, _, _, _)) _ = x

instance Indexed (ControlMap a) CK'Down a where
    from (ControlMap (_, x, _, _)) _ = x

instance Indexed (ControlMap a) CK'Left a where
    from (ControlMap (_, _, x, _)) _ = x

instance Indexed (ControlMap a) CK'Right a where
    from (ControlMap (_, _, _, x)) _ = x

instance Functor ControlMap where
    fmap f (ControlMap (x1, x2, x3, x4)) =
        ControlMap (f x1, f x2, f x3, f x4)

instance C.Controls (ControlMap C.TriggerAggregate) where
    updateControls mp win =
        sequenceCtrl $ (flip update) win <$> mp
