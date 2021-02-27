module Lib where
import Data.HashMap.Strict as H (HashMap, empty, fromList, insert, lookup, union)


--- Data Types
--- ----------

--- ### Environments and Results

type Env  = H.HashMap String Val
type PEnv = H.HashMap String Stmt

type Result = (String, PEnv, Env)

--- ### Values

data Val = IntVal Int
         | BoolVal Bool
         | CloVal [String] Exp Env
         | ExnVal String
    deriving (Eq)

instance Show Val where
    show (IntVal i) = show i
    show (BoolVal i) = show i
    show (CloVal xs body env) = "<" ++ show xs   ++ ", "
                                    ++ show body ++ ", "
                                    ++ show env  ++ ">"
    show (ExnVal s) = "exn: " ++ s

--- ### Expressions

data Exp = IntExp Int
         | BoolExp Bool
         | FunExp [String] Exp
         | LetExp [(String,Exp)] Exp
         | AppExp Exp [Exp]
         | IfExp Exp Exp Exp
         | IntOpExp String Exp Exp
         | BoolOpExp String Exp Exp
         | CompOpExp String Exp Exp
         | VarExp String
    deriving (Show, Eq)

--- ### Statements

data Stmt = SetStmt String Exp
          | PrintStmt Exp
          | QuitStmt
          | IfStmt Exp Stmt Stmt
          | ProcedureStmt String [String] Stmt
          | CallStmt String [Exp]
          | SeqStmt [Stmt]
    deriving (Show, Eq)

--- Primitive Functions
--- -------------------

intOps :: H.HashMap String (Int -> Int -> Int)
intOps = H.fromList [ ("+", (+))
                    , ("-", (-))
                    , ("*", (*))
                    , ("/", (div))
                    ]

boolOps :: H.HashMap String (Bool -> Bool -> Bool)
boolOps = H.fromList [ ("and", (&&))
                     , ("or", (||))
                     ]

compOps :: H.HashMap String (Int -> Int -> Bool)
compOps = H.fromList [ ("<", (<))
                     , (">", (>))
                     , ("<=", (<=))
                     , (">=", (>=))
                     , ("/=", (/=))
                     , ("==", (==))
                     ]

--- Problems
--- ========

--- Lifting Functions
--- -----------------

liftIntOp :: (Int -> Int -> Int) -> Val -> Val -> Val
liftIntOp op (IntVal x) (IntVal y) = IntVal $ op x y
liftIntOp _ _ _ = ExnVal "Cannot lift"

liftBoolOp :: (Bool -> Bool -> Bool) -> Val -> Val -> Val
liftBoolOp op (BoolVal x) (BoolVal y) = BoolVal $ op x y
liftBoolOp _ _ _ = ExnVal "Cannot lift"

liftCompOp :: (Int -> Int -> Bool) -> Val -> Val -> Val
liftCompOp op (IntVal x) (IntVal y) = BoolVal $ op x y
liftCompOp _ _ _ = ExnVal "Cannot lift"

--- Eval
--- ----
mv1byv2 env pair = H.insert (fst pair) (eval (snd pair) env) env


eval :: Exp -> Env -> Val

--- ### Constants

eval (IntExp i)  _ = IntVal i
eval (BoolExp i) _ = BoolVal i

--- ### Variables

eval (VarExp s) env = 
    case H.lookup s env of 
        Just s -> s
        Nothing -> ExnVal "No match in env"

--- ### Arithmetic

eval (IntOpExp op e1 e2) env | (op == "/") && (eval e2 env == IntVal 0) = ExnVal "Division by 0"
    |otherwise = 
        let v1 = eval e1 env
            v2 = eval e2 env
            Just f = H.lookup op intOps
          in liftIntOp f v1 v2

--- ### Boolean and Comparison Operators

eval (BoolOpExp op e1 e2) env = 
    let v1 = eval e1 env
        v2 = eval e2 env
        Just f = H.lookup op boolOps
          in liftBoolOp f v1 v2

eval (CompOpExp op e1 e2) env =
    let v1 = eval e1 env
        v2 = eval e2 env
        Just f = H.lookup op compOps
          in liftCompOp f v1 v2

--- ### If Expressions
--eval (IfExp (BoolExp e1) e2 e3) env = undefined
eval (IfExp e1 e2 e3) env | eval e1 env == (BoolVal True) =  eval e2 env
                          | eval e1 env == (BoolVal False) = eval e3 env
eval (IfExp _ _ _) env = ExnVal "Condition is not a Bool"



--- ### Functions and Function Application

eval (FunExp params body) env = CloVal params body env

eval (AppExp e1 args) env = undefined
--    let CloVal params body clenv = eval e1 env
--        arg = eval args env
--     in eval $ body $ ((params,arg):clenv)

--- ### Let Expressions

eval (LetExp [] body) env = eval body env

eval (LetExp pairs body) env = 
    let v1 = map (mv1byv2 env) pairs
        in eval body (last(map (H.union (head v1)) (tail v1)))

--- Statements
--- ----------

-- Statement Execution
-- -------------------

exec :: Stmt -> PEnv -> Env -> Result
exec (PrintStmt e) penv env = (val, penv, env)
    where val = show $ eval e env

--- ### Set Statements

exec (SetStmt var e) penv env = undefined
    --case H.lookup s env of 
    --    Just s -> 
    --    Nothing -> 

--- ### Sequencing

exec (SeqStmt []) penv env = undefined

--- ### If Statements

exec (IfStmt e1 s1 s2) penv env = undefined

--- ### Procedure and Call Statements

exec p@(ProcedureStmt name args body) penv env = undefined

exec (CallStmt name args) penv env = undefined
