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

recunion [] a = a
recunion (x:xs) a = recunion xs (H.union x a)  


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

eval (AppExp (FunExp _ body) []) env = eval body env

eval (AppExp (FunExp (param:params) body) (val:vals)) env = eval (AppExp (FunExp params body) vals) (H.insert param (eval val env) env)
eval (AppExp  (VarExp e1) vals) env =
    let CloVal params body env1 = eval (VarExp e1) env
    in eval (AppExp (FunExp params body) vals) env1
eval (AppExp _ _) env = ExnVal "Apply to non-closure"
--eval (AppExp e1 []) env =
--    let CloVal params body env1 = eval e1 env
--    in eval body env

--eval (AppExp e1 (val:vals)) env = 
--    let CloVal (param:params) body env1 = eval e1 env
--    in eval (AppExp (FunExp params body) vals) (H.insert param (eval val env) env)




--- ### Let Expressions

eval (LetExp [] body) env = eval body env

eval (LetExp (pair:pairs) body) env = 
    eval (LetExp (pairs) body) (H.insert (fst pair) (eval (snd pair) env) env)


--- Statements
--- ----------
seqtailhelp [] a penv env = a
seqtailhelp ((PrintStmt x):xs) a penv env = seqtailhelp xs (a ++ (show $ eval x env)) penv env
seqtailhelp ((SetStmt var x):xs) a penv env = seqtailhelp xs (a) penv env
seqtailhelp ((SeqStmt x):xs) a penv env = seqtailhelp xs (a ++ (seqtailhelp x "" penv env)) penv env
--seqtailhelp ((IfStmt  e1 s1 s2):xs) a penv env
--    | eval e1 env == BoolVal True = seqtailhelp xs (a ++ exec s1 penv env) penv env
--    | eval e1 env == BoolVal True = seqtailhelp xs (a ++ exec s2 penv env) penv env
--    | otherwise = seqtailhelp xs (a ++ "exn: Condition is not a Bool") penv env


-- Statement Execution
-- -------------------

exec :: Stmt -> PEnv -> Env -> Result
exec (PrintStmt e) penv env = (val, penv, env)
    where val = show $ eval e env

--- ### Set Statements

exec (SetStmt var e) penv env = 
    let e1 = eval e env
        env1 = H.insert var e1 env
     in ("", penv, env1)

--- ### Sequencing

exec (SeqStmt []) penv env = 
    let x = ""
    in (x, penv, env)

exec (SeqStmt (x:xs)) penv env = 
    let y = seqtailhelp  (x:xs) "" penv env
    in (y,penv,env)


--exec (SeqStmt x:xs) penv env = ("", penv, env)

--- ### If Statements

exec (IfStmt e1 s1 s2) penv env
    | eval e1 env == BoolVal True = exec s1 penv env
    | eval e1 env == BoolVal False = exec s2 penv env
    | otherwise = ("exn: Condition is not a Bool", penv, env)

--- ### Procedure and Call Statements

exec p@(ProcedureStmt name args body) penv env = ("", (H.insert name p penv), env)

exec (CallStmt name args) penv env = 
    case H.lookup name penv of 
        Just name -> (val, penv, env)
            where val = AppExp (FunExp args name) env)
        Nothing -> ("Procedure " ++ name ++ " undefined",penv,env)