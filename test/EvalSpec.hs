module EvalSpec where
import           AST
import           Eval
import           RIO
import qualified RIO.Map    as Map
import           RIO.Text
import           Test.Hspec

spec :: Spec
spec = do
    describe "eval error test" $ do
        it "undefined var error" $
            run (Var "a") `shouldReturn` Left (UndefinedVar "a")
        it "expect function error" $
            run (App (LInt 1) (LInt 1)) `shouldReturn` Left (ExpectedType Function)
        it "expect Int error" $
            run (BiOp "+" (LBool True) (LInt 1)) `shouldReturn` Left
            (ExpectedType TInt)
        it "not defined BiOp error" $
            run (BiOp "undefinedBiOp" (LInt 1) (LInt 1)) `shouldReturn` Left (UndefinedBiOp "undefinedBiOp")
    describe "eval test" $ do
        it "eval BiOp + test" $
            run (BiOp "+" (LInt 1) (LInt 1)) `shouldReturn` Right (VInt 2)
        it "eval apply test" $
            run (App (Lam "x" (Var "x")) (LInt 1)) `shouldReturn` Right (VInt 1)
        it "eval test 1" $
            run (App (App (Lam "x" (Lam "y" (BiOp "+" (Var "x") (Var "y"))))(LInt 2)) (LInt 1)) `shouldReturn` Right (VInt 3)

run :: Expr -> IO (Either EvalException Value)
run expr = runRIO Map.empty $ try (eval expr)
