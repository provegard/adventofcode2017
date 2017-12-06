module Captcha where

-- '0' is ASCII 48
to_int ch = do
  let i = fromEnum ch - 48
  if (0 <= i && i < 10) then i else error ("not a digit: " ++ show ch)

char_tuples str = do
  let half_len = length str `div` 2
  zip str ((drop half_len str) ++ (take half_len str))

is_symmetric_tuple tup = (fst tup) == (snd tup)

tuple_term tup = to_int (fst tup)

symmetric_tuples str = filter is_symmetric_tuple (char_tuples str)


digit_sum :: String -> Int
digit_sum str = sum (map tuple_term (symmetric_tuples str))

