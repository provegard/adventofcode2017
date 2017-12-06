module Passphrase where
import Data.List
import Data.Maybe

--non_empty x = (length x) > 0
--words line = filter non_empty (splitOneOf " \t" line)

are_anagrams w1 w2 = (sort w1) == (sort w2)

pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

is_valid passphrase = do
  let w = words passphrase
  let ps = pairs w
  let ana = find (\ p -> are_anagrams (fst p) (snd p)) ps
  isNothing ana

cnt passphrase = if (is_valid passphrase) then 1 else 0

count_valid passphrases = sum (map cnt passphrases)
