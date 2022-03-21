data LExpr = Var Char | Lambda Char LExpr | App LExpr LExpr 

instance Show (LExpr) where
    show (Var x) = show x
    show (Lambda x expr) = "L" ++ (show x) ++ "." ++ (show expr)
    show (App xpr ypr) = (show xpr) ++ " " ++ (show ypr) 