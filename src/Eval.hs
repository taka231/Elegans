module Eval  where
import           AST      (Env, Expr (..), Types (..), Value (..))
import           RIO
import           RIO.Map  as Map
import           RIO.Text (Text)

data EvalException = UndefinedVar Text
                    | UndefinedBiOp Text
                    | ExpectedType Types
                        deriving (Eq, Show)

instance Exception EvalException

-- >>> runRIO Map.empty $ try $ eval (BiOp "+" (LBool True) (LInt 2)) :: IO (Either SomeException Value)
-- Left (ExpectedType TInt)
eval ::  Expr -> RIO Env Value
eval (LInt num)   = pure $ VInt num
eval (LBool bool) = pure $ VBool bool
eval (Var name) = do
    env <- ask
    case Map.lookup name env of
      Nothing  -> throwM $ UndefinedVar name
      Just var -> pure var
eval (Lam arg expr) = do
    env <- ask
    pure $ Func env arg expr
eval (App expr1 expr2) = do
    val1 <- eval expr1
    val2 <- eval expr2
    case val1 of
      Func env arg body -> local (const (Map.insert arg val2 env)) $ eval body
      _                 -> throwM $ ExpectedType Function
eval (BiOp "+" expr1 expr2) = do
    val1 <- eval expr1
    val2 <- eval expr2
    case (val1, val2) of
        (VInt num1, VInt num2) -> pure $ VInt (num1 + num2)
        _                      -> throwM $ ExpectedType TInt
eval (BiOp op _ _) = throwM $ UndefinedBiOp op
