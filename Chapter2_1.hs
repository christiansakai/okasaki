module Chapter2_1 where

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


-- Figure 2.2. Implementation of stacks
-- using the built-in type of lists

instance Stack [] where
  -- empty :: [a]
  empty = []

  -- isEmpty :: [a] -> Bool
  isEmpty [] = True

  -- cons :: a -> [a] -> [a]
  cons x xs = x:xs

  -- head :: [a] -> a
  head []     = error "EMPTY"
  head (x:xs) = x

  -- tail :: [a] -> [a]
  tail []     = error "EMPTY"
  tail (x:xs) = xs

  -- (++) :: [a] -> [a] -> [a]
  [] ++ ys      = ys
  (x:xs) ++ ys  = x : (xs ++ ys)

  -- update  :: s a -> Int -> a -> s a
  update [] _ _       = error "SUBSCRIPT" 
  update (x:xs) 0 y   = y : xs 
  update (x:xs) pos y = update xs (pos - 1) y

  -- suffixes :: s a -> [s a]
  suffixes []           = [[]]
  suffixes stack@(x:xs) = stack : (suffixes xs)
 

-- Figure 2.3. Implementation of stacks
-- using a custom data type


data CustomStack a 
  = Nil
  | Cons a (CustomStack a)
  deriving Show

instance Stack CustomStack where
  -- empty :: CustomStack a
  empty = Nil

  -- isEmpty :: CustomStack a -> Bool
  isEmpty Nil = True
  isEmpty _   = False

  -- cons :: a -> CustomStack a -> CustomStack a
  cons x xs = Cons x xs

  -- head :: CustomStack a -> a
  head Nil        = error "EMPTY"
  head (Cons x _) = x

  -- tail :: [a] -> [a]
  tail Nil          = error "EMPTY"
  tail (Cons _ xs)  = xs

  -- (++) :: CustomStack a -> CustomStack a -> CustomStack a
  Nil ++ ys         = ys
  (Cons x xs) ++ ys = Cons x (xs ++ ys)

  -- update  :: s a -> Int -> a -> s a
  update Nil _ _            = error "SUBSCRIPT" 
  update (Cons x xs) 0 y    = Cons y xs 
  update (Cons x xs) pos y  = update xs (pos - 1) y

  -- suffixes :: s a -> s (s a)
  suffixes Nil                = Cons Nil Nil
  suffixes stack@(Cons x xs)  = Cons stack (suffixes xs)
