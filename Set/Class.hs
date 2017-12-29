{-# LANGUAGE MultiParamTypeClasses #-}

module Set.Class where

-- Figure 2.7. Signature for sets.
class Set s a where
  empty   :: s a

  -- Figure 2.8. Execution of ys = insert("e", xs).
  insert  :: a -> s a -> s a
  member  :: a -> s a -> Bool

