module Stack.Class where

import Prelude hiding ((++))

-- Figure 2.1. Signature for stacks

class Stack s where
  empty   :: s a
  isEmpty :: s a -> Bool
  cons    :: a -> s a -> s a
  head    :: s a -> a
  tail    :: s a -> s a

  -- Figure 2.5. Executing zs = xs ++ ys
  -- in a functional setting.
  -- Notice that the argument lists, xs and ys,
  -- are unaffected by the operation.
  (++)    :: s a -> s a -> s a

  -- Figure 2.6. Executing ys = update(xs, 2, 7).
  -- Note the sharing between xs and ys.
  update  :: s a -> Int -> a -> s a

  -- Exercise 2.1. Write a function suffixes of type
  -- s a -> s a that takes a list xs and returns a list of
  -- all the suffixes of xs in decreasing order.
  suffixes :: s a -> s (s a)
