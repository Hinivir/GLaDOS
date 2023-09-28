import Test.HUnit
import TestSExpr (printTreeTests, testSExpr)
import TestAst (
  testSExprToAst,
  testEvalAdd,
  testEvalSub,
  testEvalMul,
  testEvalDiv,
  testsEvalAst
  )

main :: IO ()
main = do
  putStrLn "Run test for SExpr"
  _ <- runTestTT testSExpr
  putStrLn "Run test for printTree"
  _ <- runTestTT printTreeTests
  putStrLn "Run test for SExpr to Ast"
  _ <- runTestTT testSExprToAst
  putStrLn "Run test for EvalAdd"
  _ <- runTestTT testEvalAdd
  putStrLn "Run test for EvalSub"
  _ <- runTestTT testEvalSub
  putStrLn "Run test for EvalMul"
  _ <- runTestTT testEvalMul
  putStrLn "Run test for EvalDiv"
  _ <- runTestTT testEvalDiv
  putStrLn "Run test for EvalAst"
  _ <- runTestTT testsEvalAst
  return ()
