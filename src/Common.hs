module Common where

---------------------
-- Utility functions
--------------------
abort :: String -> a
abort s = error ("INTERNAL ERROR: " ++ s)

infixl 1 |>
(|>) :: a -> (a -> b) -> b
x |> f = f x
