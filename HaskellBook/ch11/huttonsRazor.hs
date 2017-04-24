data Expr
  = Lit Integer
  | Add Expr Expr

-- eval (Add (Lit 1) (Lit 9001))
-- 9002
eval :: Expr -> Integer
eval (Lit z) = z
eval (Add exprA exprB) = eval exprA + eval exprB

-- printExpr (Add (Lit 1) (Lit 9001))
-- "1 + 9001"
printExpr :: Expr -> String
printExpr (Lit z) = show z
printExpr (Add exprA exprB) = printExpr exprA ++ " + " ++ printExpr exprB
