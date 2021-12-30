module EvalSpec where
import           AST
import qualified Data.Map   as M
import           Data.Text
import           Eval
import           Test.Hspec


spec :: Spec
spec = do
    describe "eval error test" $ do
        it "undefined var error" $
            runEvalForTest (Var "a") `shouldReturn` Left "cannot reach var: a"
        it "expect function error" $
            runEvalForTest (App (LInt 1) (LInt 1)) `shouldReturn` Left "expect function"
        it "expect Int error" $
            runEvalForTest (BiOp "+" (LBool True) (LInt 1)) `shouldReturn` Left "expect Int"
        it "not defined BiOp error" $
            runEvalForTest (BiOp "undefinedBiOp" (LInt 1) (LInt 1)) `shouldReturn` Left "Binary Operator: undefinedBiOp is not defined"
    describe "eval test" $ do
        it "eval BiOp + test" $
            runEvalForTest (BiOp "+" (LInt 1) (LInt 1)) `shouldReturn` Right (VInt 2)
        it "eval apply test" $
            runEvalForTest (App (Lam "x" (Var "x")) (LInt 1)) `shouldReturn` Right (VInt 1)
        it "eval test 1" $
            runEvalForTest (App (App (Lam "x" (Lam "y" (BiOp "+" (Var "x") (Var "y"))))(LInt 2)) (LInt 1)) `shouldReturn` Right (VInt 3)

runEvalForTest :: Expr -> IO (Either Text Value)
runEvalForTest expr = runEval M.empty (eval expr)
