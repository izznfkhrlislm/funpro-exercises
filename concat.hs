concatenate [] = []
concatenate (xs: xss) = xs ++ concatenate xss