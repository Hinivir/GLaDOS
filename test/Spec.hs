import Test.HUnit
import TestSExpr (printTreeTests, testSExpr)
import TestAst (testSExprToAst, testsEvalAst)

main :: IO ()
main = do
  _ <- runTestTT testSExpr
  _ <- runTestTT printTreeTests
  _ <- runTestTT testSExprToAst
  _ <- runTestTT testsEvalAst
  return ()
