data Expr = C Float | Expr :+ Expr | Expr :- Expr | Expr :* Expr | Expr :/ Expr
            | V String | Let String Expr Expr
    deriving Show

{-
    subst
    Input: String, Expr, Expr
    Output: Expr
-}
subst :: String -> Expr -> Expr -> Expr
subst v0 e0 (V v1) = if (v0 == v1) then e0 else (V v1)
subst _ _ (C c) = (C c)
subst v0 e0 (e1 :+ e2) = subst v0 e0 e1 :+ subst v0 e0 e2
subst v0 e0 (e1 :- e2) = subst v0 e0 e1 :- subst v0 e0 e2
subst v0 e0 (e1 :* e2) = subst v0 e0 e1 :* subst v0 e0 e2
subst v0 e0 (e1 :/ e2) = subst v0 e0 e1 :/ subst v0 e0 e2
subst v0 e0 (Let v1 e1 e2) = Let v1 e1 (subst v0 e0 e2)

evaluate :: Expr -> Float
evaluate (C x) = x
evaluate (e1 :+ e2) = evaluate e1 + evaluate e2
evaluate (e1 :- e2) = evaluate e1 - evaluate e2
evaluate (e1 :* e2) = evaluate e1 * evaluate e2
evaluate (e1 :/ e2) = evaluate e1 / evaluate e2
evaluate (Let v e0 e1) = evaluate (subst v e0 e1)
evaluate (V _) = 0.0

-- Imperative Robot
data Direction = North | East | South | West
    deriving (Eq, Show, Enum)

right :: Direction -> Direction
right d = toEnum (succ (fromEnum d) `mod` 4)

data RobotState = RobotState { position :: Position , facing :: Direction
                                , pen :: Bool , color :: Color
                                , treasure :: [Position] , pocket :: Int
                             } deriving Show
newtype Robot a = Robot (RobotState -> Grid -> Window -> IO (RobotState, a))

turnRight :: Robot ()
turnRight = updateState (\s -> s {facing = right (facing s)})
moven :: Int -> Robot ()
moven n = mapM_ (const move) [ 1 .. n ]