import Data.List

[toUpper c | c <- s]

>>> x = [2,3,4] \\ [3]
>>> perm [] = [[]]
>>> perm ls = [ x:ps | x <- ls, ps <- perm(ls\\[x])]
>>> perm [1,2,3]
<interactive>:51:14-15: warning: [-Wdeferred-out-of-scope-variables]
    Variable not in scope: (\\) :: [Integer] -> [Integer] -> t
<BLANKLINE>
<interactive>:52:2-15: warning: [-Wincomplete-patterns]
    Pattern match(es) are non-exhaustive
    In an equation for ‘perm’: Patterns not matched: (_:_)
<BLANKLINE>
<interactive>:53:21: warning: [-Wname-shadowing]
    This binding for ‘x’ shadows the existing binding
      defined at <interactive>:51:2
<BLANKLINE>
<interactive>:53:43-44: warning: [-Wdeferred-out-of-scope-variables]
    Variable not in scope: (\\) :: [a] -> [a] -> [a]
*** Exception: <interactive>:53:43-44: error:
    Variable not in scope: (\\) :: [a] -> [a] -> [a]
(deferred type error)

>>> add x y = x + y
>>> add 16 5
21

>>> [1,2,3,4] ++ [5,6,7,8]
>>> 5 : [1,2,3,4]
[1,2,3,4,5,6,7,8]
[5,1,2,3,4]



