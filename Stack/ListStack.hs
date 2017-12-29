module Stack.ListStack where

import Stack.Class
import Prelude hiding ((++))

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
 
