module Passphrase where
import Data.List

--non_empty x = (length x) > 0
--words line = filter non_empty (splitOneOf " \t" line)

is_valid passphrase = do
  let w = words passphrase
  (length w) == (length (nub w))

cnt passphrase = if (is_valid passphrase) then 1 else 0

count_valid passphrases = sum (map cnt passphrases)
