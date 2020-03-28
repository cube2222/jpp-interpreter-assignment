let Ok e = pExpr (myLexer "(2+2)*5") in e
startEnv
calc "(2+2)*5"
pExpr (myLexer "val x = 6; val y = 2; 2")
withVariable (Ident "test") (Integer 5) startEnv
calc "val x = 6; val y = 2; x * y"
