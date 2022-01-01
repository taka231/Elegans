module AST where
import           RIO
import           RIO.Map  (Map)
import           RIO.Text (Text)

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

data Types = TInt
            | Function
            deriving (Eq,Show)
