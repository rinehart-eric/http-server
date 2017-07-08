test [] = []
test [13] = []
test (13:13:xs) = test (13:xs)
test (13:x:xs) = test xs
test (x:xs) = x:test xs
