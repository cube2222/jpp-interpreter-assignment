import Interpreter

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

testCases = [
    ("eval 5", "5", Right (Integer 5)),
    ("eval true", "true", Right (Bool True)),
    ("basic arithmetics", "5+3+4*2-(4/2)", Right (Integer 14)),
    ("division by zero", "5+3+4*2-(4/0)", Left (CalcIError IErrDivisionByZero))
    ]