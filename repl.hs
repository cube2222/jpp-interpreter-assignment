let Ok e = pExpr (myLexer "(2+2)*5") in e
startEnv
calc "(2+2)*5"
pExpr (myLexer "val x = 6; val y = 2; 2")
calc "val x = 6; val y = 2; 2"
calc "val x = 6; val y = 2; x * y"
