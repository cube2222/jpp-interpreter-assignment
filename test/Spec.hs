import Interpreter
import Language.Abs
import Language.Lex
import Language.Par
import Language.ErrM

main :: IO ()
main = do
    runTests
    return ()

runTests :: IO ()
runTests = foldl (\acc -> (\test -> acc >> (runTestCase test))) (pure ()) testCases

runTestCase (name, program, want) = do
    let actual = calc program
    putStrLn (name ++ ": " ++ (if actual == want then "Ok" else ("Failed: '" ++ (show want) ++ "' != '" ++ (show actual) ++ "'")))
    return ()

parseExpr str = let (Ok expr) = (pExpr (myLexer str)) in clearM expr

testCases = [
    ("eval 5", "5", Right (Integer 5)),
    ("eval true", "true", Right (Bool True)),
    ("basic arithmetics", "5+3+4*2-(4/2)", Right (Integer 14)),
    ("division by zero", "5+3+4*2-(4/0)", Left (CalcIError IErrDivisionByZero)),
    ("compare", "5 < 2", Right (Bool False)),
    ("compare", "5 <= 2", Right (Bool False)),
    ("compare", "2 <= 5", Right (Bool True)),
    ("compare", "5 >= 5", Right (Bool True)),
    ("compare", "5 > 5", Right (Bool False)),
    ("compare", "5 == 5", Right (Bool True)),
    ("compare", "4 == 5", Right (Bool False)),
    ("compare", "5 != 5", Right (Bool False)),
    ("compare", "4 != 5", Right (Bool True)),
    ("compare", "true == true", Right (Bool True)),
    ("compare", "true == false", Right (Bool False)),
    ("compare", "true != false", Right (Bool True)),
    ("if then else", "if true then 5 else 4", Right (Integer 5)),
    ("if then else", "if false then 5 else 4", Right (Integer 4)),
    ("if then else", "if 4 < 5 then 5 else 4", Right (Integer 5)),
    ("if then else", "if 4 < 5 then (if 3 == 3 then 7 else 2) else 4", Right (Integer 7)),
    ("if then else", "if 4 + 5 then 5 else 4", Left (CalcTCError $ TCInvalidExpressionType (parseExpr "4+5") [TBool] TInteger)),
    ("if then else", "if true then 5 else false", Left (CalcTCError $ TCInvalidExpressionType (parseExpr "false") [TInteger] TBool)),
    ("variables", "val x = 3; x", Right (Integer 3)),
    ("variables", "val x = 3; val y = x + 3; x + y", Right (Integer 9)),
    ("variables", "if (val x = 3; val y = x + 3; y > x) then 4 else 3", Right (Integer 4)),
    ("variables", "if (val x = 3; val y = x + 3; y) then 4 else 3", Left (CalcTCError $ TCInvalidExpressionType (parseExpr "val x = 3; val y = x + 3; y") [TBool] TInteger)),
    ("variables", "val x = 3; y", Left (CalcTCError $ TCUnknownVariable (Ident "y"))),
    ("function", "fun test(x: Int): Int {val y = x + 2; y}; test(3)", Right (Integer 5)),
    ("function", "fun test(x: Int, y: Int): Int {val y = x + y + 2; y}; test(3,2)", Right (Integer 7)),
    ("function", "fun test(x: Int, y: Int): Int {val y = x + y + 2; true}; test(3,2)", Left (CalcTCError $ TCInvalidExpressionType (parseExpr "val y = x + y + 2; true") [TInteger] TBool)),
    ("recursion", 
        "fun test(x: Int, y: Int): Int {    \
        \    if x > 0                       \
        \    then 1 + test(x-1, y)          \
        \    else if y > 0                  \
        \         then 1 + test(y-1, y-1)   \
        \         else 0                    \
        \};                                 \
        \test(3,2)",
        Right (Integer 6)),
    ("recursion", 
        "fun test(x: Int, y: Int): Int {    \
        \    if x > 0                       \
        \    then 1 + test(x-1, y)          \
        \    else if test(y-1, y-1)         \
        \         then 1                    \
        \         else 0                    \
        \};                                 \
        \test(3,2)",
        Left (CalcTCError $ TCInvalidExpressionType (parseExpr "test(y-1, y-1)") [TBool] TInteger))
    ]