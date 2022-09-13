module Conditionals where

dbzDialogue :: Integer -> String
dbzDialogue powerLevel = if powerLevel < 9000
                            then "Weak"
                            else "It's over 9000!!!"

-- nesting
signum :: Int -> Int
signum n = if n < 0 then -1 else
            if n == 0 then 0 else 1
                            