module Eval where
import           AST                  (Env, Expr (..), Value (..))
import           Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import           Control.Monad.Reader (MonadReader (..), ReaderT (runReaderT))
import           Data.Map             as M
import           Data.Text            (Text)

type Eval a = ReaderT Env (ExceptT Text IO) a
runEval :: Env -> Eval Value -> IO (Either Text Value)
runEval env ev = runExceptT (runReaderT ev env)

eval :: Expr -> Eval Value
eval (LInt num)   = pure $ VInt num
eval (LBool bool) = pure $ VBool bool
eval (Var name) = do
    env <- ask
    case M.lookup name env of
      Nothing  -> throwError $ "cannot reach var: " <> name
      Just var -> pure var
eval (Lam arg expr) = do
    env <- ask
    pure $ Func env arg expr
eval (App expr1 expr2) = do
    val1 <- eval expr1
    val2 <- eval expr2
    case val1 of
      Func env arg body -> local (const (M.insert arg val2 env)) $ eval body
      _                 -> throwError "expect function"
eval (BiOp "+" expr1 expr2) = do
    val1 <- eval expr1
    val2 <- eval expr2
    case (val1, val2) of
        (VInt num1, VInt num2) -> pure $ VInt (num1 + num2)
        _                      -> throwError "expect Int"
eval (BiOp op _ _) = throwError $ "Binary Operator: " <> op <> " is not defined"



