module AST where
import           Data.Map  (Map)
import           Data.Text (Text)

data Expr = LInt Int
    | LBool Bool
    | Lam Text Expr
    | App Expr Expr
    | Var Text
    | BiOp Text Expr Expr
    deriving (Eq, Show)

data Value = VInt Int
    | VBool Bool
    | Func Env Text Expr
    deriving (Eq, Show)

type Env = Map Text Value
