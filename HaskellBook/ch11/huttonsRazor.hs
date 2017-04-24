data Expr
  = Lit Integer
  | Add Expr Expr

-- eval (Add (Lit 1) (Lit 9001))
-- 9002
eval :: Expr -> Integer
eval = error "todo"

-- printExpr (Add (Lit 1) (Lit 9001))
-- "1 + 9001"
printExpr :: Expr -> String
printExpr = undefined
