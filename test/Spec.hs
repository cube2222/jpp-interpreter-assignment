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
parseMatchClause str = let (Ok clause) = (pMatchClause (myLexer str)) in clearM clause

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
    ("if then else", "if 4 + 5 then 5 else 4", Left (CalcTCError $ (1, TCInvalidExpressionType (parseExpr "4+5") [TBool] TInteger))),
    ("if then else", "if true then 5 else false", Left (CalcTCError $ (1, TCInvalidExpressionType (parseExpr "false") [TInteger] TBool))),
    ("variables", "val x = 3; x", Right (Integer 3)),
    ("variables", "val x = 3; val y = x + 3; x + y", Right (Integer 9)),
    ("variables", "if (val x = 3; val y = x + 3; y > x) then 4 else 3", Right (Integer 4)),
    ("variables", "if (val x = 3; val y = x + 3; y) then 4 else 3", Left (CalcTCError $ (1, TCInvalidExpressionType (parseExpr "val x = 3; val y = x + 3; y") [TBool] TInteger))),
    ("variables", "val x = 3; y", Left (CalcTCError $ (1, TCUnknownVariable (Ident "y")))),
    ("function", "fun test(x: Int): Int {val y = x + 2; y}; test(3)", Right (Integer 5)),
    ("function", "fun test(x: Int, y: Int): Int {val y = x + y + 2; y}; test(3,2)", Right (Integer 7)),
    ("function", "fun test(x: Int, y: Int): Int {val y = x + y + 2; true}; test(3,2)", Left (CalcTCError $ (1, TCInvalidExpressionType (parseExpr "val y = x + y + 2; true") [TInteger] TBool))),
    ("recursion", 
        "fun test(x: Int, y: Int): Int {    \n\
        \    if x > 0                       \n\
        \    then 1 + test(x-1, y)          \n\
        \    else if y > 0                  \n\
        \         then 1 + test(y-1, y-1)   \n\
        \         else 0                    \n\
        \};                                 \n\
        \test(3,2)",
        Right (Integer 6)),
    ("recursion", 
        "fun test(x: Int, y: Int): Int {    \n\
        \    if x > 0                       \n\
        \    then 1 + test(x-1, y)          \n\
        \    else if test(y-1, y-1)         \n\
        \         then 1                    \n\
        \         else 0                    \n\
        \};                                 \n\
        \test(3,2)",
        Left (CalcTCError $ (4, TCInvalidExpressionType (parseExpr "test(y-1, y-1)") [TBool] TInteger))),
    ("anonymous functions", "val f = (x: Int -> x + 2); f(3)", Right (Integer 5)),
    ("anonymous functions", "(x: Int -> x + 2)(3)", Right (Integer 5)),
    ("partial application", "(x: Int -> (y: Int -> x + 2*y + 2))(3)(4)", Right (Integer 13)),
    ("partial application", "(x: Int -> (y: Int -> x + 2*y + 2))(3, 4)", Right (Integer 13)),
    ("partial application", "val partial = (x: Int -> (y: Int -> x + 2*y + 2))(3); partial(4)", Right (Integer 13)),
    ("partial application", 
    "fun test(x: Int, y: Int): Int {x + 2*y + 2}; val partial = test(3); partial(4)", 
    Right (Integer 13)),
    ("partial application", 
    "fun test(x: Int, y: Int): Int {x + 2*y + 2}; test(3)(4)", 
    Right (Integer 13)),
    ("partial application", 
    "fun test(x: Int, y: Int): Int {x + 2*y + 2}; test(3,4)", 
    Right (Integer 13)),
    ("partial application", 
    "fun test(x: Int, y: Int): Int {x + 2*y + 2}; test(3,4,5)", 
    Left (CalcTCError $ (1, TCNonFunctionCall (parseExpr "test(3,4,5)") TInteger))),
    ("partial application", 
    "fun test(x: Int, y: Int): Int {x + 2*y + 2}; test(3,true)", 
    Left (CalcTCError $ (1, TCInvalidExpressionType (parseExpr "true") [TInteger] TBool))),
    ("higher order functions", 
    "fun apply2(f: Fun<Int, Int>): Int {f(2)}; apply2((x: Int -> x+2))", 
    Right (Integer 4)),
    ("higher order functions", 
    "fun add(x: Int, y: Int): Int { x + 2*y };                  \n\
    \fun apply2and2(f: Fun<Int, Fun<Int, Int>>): Int {f(2,3)};  \n\
    \apply2and2(add)", 
    Right (Integer 8)),
    ("runtime error", "5/0", Left (CalcIError IErrDivisionByZero)),
    ("pattern matching", 
    "match [1,2,3]          \n\
    \    as x :: xs ~> x    \n\
    \    as [] ~> -1", 
    Right (Integer 1)),
    ("pattern matching", 
    "match []               \n\
    \    as x :: xs ~> x    \n\
    \    as [] ~> -1", 
    Left (CalcTCError $ (1, TCImpossibleMatchClause (parseMatchClause "as x :: xs ~> x") (parseExpr "match [] as x :: xs ~> x as [] ~> -1")))),
    ("pattern matching", 
    "match [1]                  \n\
    \    as x :: y :: xs ~> x   \n\
    \    as [] ~> -1", 
    Left (CalcIError $ IErrUnexhaustiveMatch)),
    ("pattern matching", 
    "match [[1],[2,3,4],[3]]                \n\
    \    as a :: [x,y,z] :: tail ~> x+y+z   \n\
    \    as a ~> -1", 
    Right (Integer 9)),
    ("pattern matching", 
    "match [[1],[2,3,4,4],[3]]                \n\
    \    as a :: [x,y,z] :: tail ~> x+y+z   \n\
    \    as a ~> -1", 
    Right (Integer (-1))),
    ("list syntax sugar", "[1,2,3]", Right (List [Integer 1, Integer 2, Integer 3])),
    ("list syntax sugar", "1 :: 2 :: 3 :: nil", Right (List [Integer 1, Integer 2, Integer 3])),
    ("list syntax sugar", "1 :: 2 :: 3 :: []", Right (List [Integer 1, Integer 2, Integer 3])),
    ("list syntax sugar", "1 :: 2 :: [3]", Right (List [Integer 1, Integer 2, Integer 3])),
    ("list syntax sugar", "1 :: [2, 3]", Right (List [Integer 1, Integer 2, Integer 3])),
    ("bool list", "[true, false]", Right (List [Bool True, Bool False])),
    ("list type error", "[1,true,3]", Left (CalcTCError $ (1, TCInvalidExpressionType (parseExpr "true") [TInteger] TBool))),
    ("list type error", "1 :: true :: 3 :: nil", Left (CalcTCError $ (1, TCInvalidExpressionType (parseExpr "3 :: nil") [TNil, TList TBool] (TList TInteger)))),
    ("nested list", "[[1,2], [1], []]", Right (List [List [Integer 1, Integer 2], List [Integer 1], List []])),
    ("function list", 
    "fun head(x: List<Fun<Int, Int>>): Fun<Int, Int> {  \n\
    \    match x                                        \n\
    \        as f :: fs ~> f                            \n\
    \};                                                 \n\
    \val fList = [(x: Int -> x + 2), (x: Int -> x + 3)];\n\
    \head(fList)(2)", 
    Right (Integer 4)),
    ("function list", 
    "fun head(x: List<Fun<Int, Int>>): Fun<Int, Int> {  \n\
    \    match x                                        \n\
    \        as f :: fs ~> f                            \n\
    \};                                                 \n\
    \val fList = [(x: Int -> x + 2), (x: Int -> x + 3)];\n\
    \head(fList, 2)", 
    Right (Integer 4)),
    ("static binding", 
    "val x = 4;                 \n\
    \val f = (y: Int -> x + y); \n\
    \val x = 3;                 \n\
    \f(x)", 
    Right (Integer 7)),
    ("static binding", 
    "val x = 4;                 \n\
    \fun f(y: Int): Int {x + y}; \n\
    \val x = 3;                 \n\
    \f(x)", 
    Right (Integer 7))
    ]