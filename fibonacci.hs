add [] [] = []
add (a:as) (b:bs) = (a + b) : (add as bs)

fibonacci = 1 : 1 : add fibonacci (tail fibonacci)