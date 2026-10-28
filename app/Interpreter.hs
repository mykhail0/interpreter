{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Interpreter where

import Data.String
import Data.Maybe

import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader

import qualified Data.Map.Lazy as Map

import AbsProgram

data MyError = MyError {
  location :: BNFC'Position,
  reason   :: String
} deriving Show

type RuntimeError = MyError

type ErrRE = Either RuntimeError

-- In addition to the state needed to evaluate an expression,
-- it holds a value to be returned.
type StmtState = (Value,ExpState)

data ExpState = ES {
  env    :: Env,
  store  :: Store,
  fenv   :: FEnv,
  fstore :: FStore
}

-- Environment maps a variable to its address in memory (represented as Int).
type Env = Map.Map String Int
-- Similar to Env.
type FEnv = Map.Map String Int
-- Memory store.
data Store = Mem {
  next :: Int,              -- The next free place in memory.
  mem  :: Map.Map Int Value -- Maps memory addresses to values.
} deriving Show
-- Similar to Store.
data FStore = FMem {
  fnext :: Int,
  fmem  :: Map.Map Int Function
}

-- 3 kinds of Values.
data Value
  = IntVal Integer
  | BoolVal Bool
  | StringVal String

instance Show Value where
  show (IntVal i) = show i
  show (BoolVal b) = show b
  show (StringVal s) = s

type Function = ([Arg],Eval Value)

data ProgState = PS {
  progEnv   :: FEnv,
  progStore :: FStore
}
type Prog a = StateT ProgState (ExceptT RuntimeError IO) a

-- Runs interpreted function definition.
runFDecl :: FunDef -> ProgState -> IO (ErrRE ProgState)
runFDecl fundef st = do
  ans <- runExceptT $ runStateT (execFDecl fundef) st
  case ans of
    (Left e)    -> return $ Left e
    (Right ans) -> return $ Right $ snd ans

-- Interprets function definition.
execFDecl :: FunDef -> Prog ()
execFDecl fun@(FunDef _ type_ (Ident ident) args block) = do
  let func = (args,evalFunction fun)
  ~(st@(PS fenv (FMem fnext fmem))) <- get
  case Map.lookup ident fenv of
    Nothing -> do
      let fenv' = Map.insert ident fnext fenv
      let fnext' = fnext + 1
      let fmem' = Map.insert fnext func fmem
      put (PS fenv' (FMem fnext' fmem'))
    Just loc -> do
      let fmem' = Map.insert fnext func fmem
      put (PS fenv (FMem fnext fmem'))

-- Before applying the function, caller first needs to load the parameters.
evalFunction :: FunDef -> Eval Value
evalFunction (FunDef l type_ _ _ block) = do
  expState@(ES env (Mem n _) fenv fstore) <- get
  ans <- liftIO $ runStmt (BStmt l block) (defRetValFromFunType type_,expState)
  case ans of
    (Left e)    -> throwError e
    (Right ans) -> do
      let (ret,ES _ (Mem _ mem) _ _) = ans
      put (ES env (Mem n mem) fenv fstore)
      return ret

-- Interpreted program statement.
type Exec a = StateT StmtState (ExceptT RuntimeError IO) a

-- Runs interpreted program statement.
runStmt :: Stmt -> StmtState -> IO (ErrRE StmtState)
runStmt stmt st = do
  ans <- runExceptT $ runStateT (execStmt stmt) st
  case ans of
    (Left e)    -> return $ Left e
    (Right ans) -> return $ Right $ snd ans

-- Interprets program statement.
execStmt :: Stmt -> Exec ()

execStmt (Empty _) = return ()

execStmt (SExp _ expr) = do
  ~(v,expState@(ES env (Mem n _) fenv fstore)) <- get
  -- Evaluate expression.
  ans <- liftIO $ runExpr expr expState
  case ans of
    (Left e)    -> throwError e
    (Right ans) -> do
      let (_,Mem _ mem) = ans
      -- Only memory for known variables might have changed.
      put (v,ES env (Mem n mem) fenv fstore)

execStmt (Ret _ expr) = do
  ~(_,expState@(ES env (Mem n _) fenv fstore)) <- get
  ans <- liftIO $ runExpr expr expState
  case ans of
    (Left e)    -> throwError e
    (Right ans) -> do
      let (evaluatedExpr,Mem _ mem) = ans
      -- New return value and memory for known variables might have changed.
      put (evaluatedExpr,ES env (Mem n mem) fenv fstore)

execStmt (Decl l type_ items) = case items of
  []                           -> return ()
  ((Init _ ident expr):items') -> do
    ~(v,expState@(ES env (Mem n _) fenv fstore)) <- get
    ans <- liftIO $ runExpr expr expState
    case ans of
      (Left e)    -> throwError e
      (Right ans) -> do
        let (evaluatedExpr,Mem _ mem) = ans
        -- Use updated memory for known variables after evaluating expr.
        let mem' = Map.insert n evaluatedExpr mem
        let env' = Map.insert (toString ident) n env
        -- New variable environment and store.
        put (v,ES env' (Mem (n + 1) mem') fenv fstore)
        execStmt (Decl l type_ items')

execStmt (Ass l ident expr) = do
  ~(v,expState@(ES env (Mem n _) fenv fstore)) <- get
  ans <- liftIO $ runExpr expr expState
  case ans of
    (Left e)    -> throwError e
    (Right ans) -> do
      let (evaluatedExpr,Mem _ mem) = ans
      let strIdent = toString ident
      case Map.lookup strIdent env of
        Nothing  -> throwError $ MyError l $ "Unbound variable: " ++ strIdent
        Just loc -> do
          let mem' = Map.insert loc evaluatedExpr mem
          -- Updated memory.
          put (v,ES env (Mem n mem') fenv fstore)

execStmt (CondElse l expr block1 block2) = do
  ~(v,expState@(ES env (Mem n _) fenv fstore)) <- get
  ans <- liftIO $ runExpr expr expState
  case ans of
    (Left e)    -> throwError e
    (Right ans) -> do
      let (BoolVal b,Mem _ mem) = ans
      put (v,ES env (Mem n mem) fenv fstore)
      execStmt $ BStmt l $ if b then block1 else block2

execStmt (Cond l expr block) = execStmt $ CondElse l expr block emptyStmt
 where
  emptyStmt :: Block
  emptyStmt = Block Nothing []

execStmt x@(While l expr block) = do
  ~(v,expState@(ES env (Mem n _) fenv fstore)) <- get
  ans <- liftIO $ runExpr expr expState
  case ans of
    (Left e)    -> throwError e
    (Right ans) -> do
      let (BoolVal b,Mem _ mem) = ans
      put (v,ES env (Mem n mem) fenv fstore)
      if b then do
        execStmt $ BStmt l block
        execStmt x
      else
        return ()

execStmt (BStmt l0 (Block l1 extStmts)) = case extStmts of
    [] -> return ()
    _  -> do
      ~(_,ES env (Mem n _) fenv fstore) <- get
      executeStatements extStmts
      ~(v,ES _ (Mem _ mem) _ _) <- get
      -- Statements in the block can only change
      -- memory for known variables and the return value.
      put (v,ES env (Mem n mem) fenv fstore)
   where
    executeStatements :: [ExtStmt] -> Exec ()
    executeStatements [] = return ()
    executeStatements ((FDecl _ fdecl):statements') = do
      ~(v,ES env store fenv fstore) <- get
      ans <- liftIO $ runFDecl fdecl $ PS fenv fstore
      case ans of
        (Left e)    -> throwError e
        (Right (PS fenv fstore)) -> do
          put (v,ES env store fenv fstore)
      executeStatements statements'
    executeStatements ((Statement _ stmt):statements') = do
      execStmt stmt
      executeStatements statements'

execStmt (Print _ expr) = do
  ~(v,expState@(ES env (Mem n _) fenv fstore)) <- get
  ans <- liftIO $ runExpr expr expState
  case ans of
    (Left e)    -> throwError e
    (Right ans) -> do
      let (valToPrint,Mem _ mem) = ans
      put (v,ES env (Mem n mem) fenv fstore)
      liftIO $ putStr $ show valToPrint

type Eval a = StateT ExpState (ExceptT RuntimeError IO) a
runExpr :: Expr -> ExpState -> IO (ErrRE (Value,Store))
runExpr expr st = do
  ans <- runExceptT $ runStateT (evalExpr expr) st
  case ans of
    (Left e)    -> return $ Left e
    (Right ans) -> return $ Right (fst ans,store $ snd ans)

evalExpr :: Expr -> Eval Value

evalExpr (EInt _ integer)   = return $ IntVal integer
evalExpr (EBool _ litbool)  = return $ BoolVal $ toBool litbool
evalExpr (EString _ string) = return $ StringVal string

evalExpr (EVar l ident) = do
  ~(ES env (Mem _ mem) _ _) <- get
  let s = toString ident
  let loc = Map.lookup s env
  case loc of
    Nothing  -> throwError $ MyError l $ "Unbound variable: " ++ s
    Just val -> return $ fromJust $ Map.lookup val mem

evalExpr (EAdd _ expr1 op expr2) = do
  e1 <- evalExpr expr1
  e2 <- evalExpr expr2
  return $ evalAdd op e1 e2
evalExpr (EMul _ expr1 op expr2) = do
  e1 <- evalExpr expr1
  e2 <- evalExpr expr2
  evalMul op e1 e2
evalExpr (ERel _ expr1 op expr2) = do
  e1 <- evalExpr expr1
  e2 <- evalExpr expr2
  return $ evalRel op e1 e2

evalExpr (ENeg _ expr) = do
  ~(IntVal i) <- evalExpr expr
  return $ IntVal (- i)
evalExpr (ENot _ expr) = do
  ~(BoolVal b) <- evalExpr expr
  return $ BoolVal $ not b

evalExpr (EAnd _ expr1 expr2) = do
  ~(BoolVal b1) <- evalExpr expr1
  ~(BoolVal b2) <- evalExpr expr2
  return $ BoolVal $ b1 && b2
evalExpr (EOr _ expr1 expr2) = do
  ~(BoolVal b1) <- evalExpr expr1
  ~(BoolVal b2) <- evalExpr expr2
  return $ BoolVal $ b1 || b2

evalExpr (EApp l (Ident ident) params) = do
  ~(ES env (Mem n _) fenv fstore@(FMem _ fmem)) <- get
  case Map.lookup ident fenv of
    Nothing  -> throwError $ MyError l $ "Unbound function: " ++ ident
    Just loc -> do {
        let {(args,evaluation) = fromJust $ Map.lookup loc fmem};
        stateSnapshot <- get;
        _ <- loadParams args params;
        ret <- evaluation;
        -- Only memory might have changed.
        modify (\(ES _ (Mem _ mem) _ _) ->
          ES env (Mem n mem) fenv fstore);
        return ret;
      }

loadParams :: [Arg] -> [Param] -> Eval ()
loadParams [] [] = return ()
loadParams (argRef@(Ref _ _ (Ident arg)):args) (paramRef@(ERef l (Ident param)):params) = do
  env <- gets env
  case Map.lookup param env of
    Nothing  -> throwError $ MyError l $ "Unbound variable: " ++ param
    Just loc -> do {
        loadParams args params;
        modify (\(ES env store fenv fstore) ->
          ES (Map.insert arg loc env) store fenv fstore);
      }
loadParams (argVar@(Var _ _ (Ident arg)):args) (paramVal@(EVal l expr):params) = do
  expState <- get;
  ans <- liftIO $ runExpr expr expState;
  case ans of
    (Left e)    -> throwError e
    (Right ans) -> do {
        let {(evaluatedExpr,Mem _ mem) = ans};
        -- Update memory for known variables after evaluating expr.
        modify (\(ES env (Mem n _) fenv fstore) ->
          ES env (Mem n mem) fenv fstore);
        loadParams args params;
        modify (\(ES env (Mem n mem) fenv fstore) ->
          ES
            (Map.insert arg n env)
            (Mem (n + 1) (Map.insert n evaluatedExpr mem))
            fenv
            fstore
        );
      }

evalAdd :: AddOp -> Value -> Value -> Value
evalAdd op i1@(IntVal _) i2@(IntVal _) = liftIII funcOp i1 i2
  where funcOp = addOpAsFunc op
evalAdd (Plus _) (StringVal s1) (StringVal s2) = StringVal $ s1 ++ s2

evalMul :: MulOp -> Value -> Value -> Eval Value
evalMul (Times _) i1 i2 = return $ liftIII (*) i1 i2
evalMul op i1 i2@(IntVal i) =
  ev (line op) (str op) (mulOpAsFunc op)
  where
    ev :: BNFC'Position -> String -> (Integer -> Integer -> Integer) -> Eval Value
    ev l opStr op
      | i == 0    = throwError $ MyError l $ opStr ++ " over 0"
      | otherwise = return $ liftIII op i1 i2
    line (Times l) = l
    line (Div l)   = l
    line (Mod l)   = l
    str (Times l)  = "*"
    str (Div l)    = "/"
    str (Mod l)    = "%"

evalRel :: RelOp -> Value -> Value -> Value
evalRel op = liftIIB $ relAsFunc op

liftIII :: (Integer -> Integer -> Integer) -> Value -> Value -> Value
liftIII op (IntVal i1) (IntVal i2) = IntVal $ i1 `op` i2

liftIIB :: (Integer -> Integer -> Bool) -> Value -> Value -> Value
liftIIB op (IntVal i1) (IntVal i2) = BoolVal $ i1 `op` i2

interpreter :: Program -> IO (ErrRE ProgState)
interpreter program = runProgram program $ PS Map.empty $ FMem 0 Map.empty

-- Runs the interpreted program.
runProgram :: Program -> ProgState -> IO (ErrRE ProgState)
runProgram program@(Program l _) state = do
  ans <- runExceptT $ runStateT (interpret program) state
  case ans of
    (Left e) -> return $ Left e
    (Right ans) -> return $ Right $ snd ans

-- Interprets the program.
interpret :: Program -> Prog ()
interpret (Program l fundefs) = let
    interpret_ :: [FunDef] -> Prog ()
    interpret_ [] = throwError $ MyError l "main() is undefined."
    interpret_ (fun@(FunDef line type_ ident@(Ident strIdent) args _):funs)
      | strIdent == "main" && null args = let
          stmt = SExp line (EApp line ident [])
          retVal = defRetValFromFunType type_
        in do
          execFDecl fun
          ~(finState@(PS fenv fstore)) <- get
          let stmtState = (retVal,ES Map.empty (Mem 0 Map.empty) fenv fstore)
          ans <- liftIO $ runStmt stmt stmtState
          case ans of
            (Left e)    -> throwError e
            (Right ans) -> return ()
      | otherwise = do
        execFDecl fun
        interpret_ funs
  in interpret_ fundefs

defRetValFromFunType :: Type -> Value
defRetValFromFunType (Int _) = IntVal 0
defRetValFromFunType (Bool _) = BoolVal False
defRetValFromFunType (Str _) = StringVal ""

toBool :: LitBool -> Bool
toBool (ETrue _)  = True
toBool (EFalse _) = False

toString :: Ident -> String
toString (Ident s) = s

relAsFunc :: RelOp -> Integer -> Integer -> Bool
relAsFunc (LTH _) = (<)
relAsFunc (LE _)  = (<=)
relAsFunc (GTH _) = (>)
relAsFunc (GE _)  = (>=)
relAsFunc (EQU _) = (==)
relAsFunc (NE _)  = (/=)

addOpAsFunc :: AddOp -> Integer -> Integer -> Integer
addOpAsFunc (Plus _)  = (+)
addOpAsFunc (Minus _) = (-)

mulOpAsFunc :: MulOp -> Integer -> Integer -> Integer
mulOpAsFunc (Times _) = (*)
mulOpAsFunc (Div _)   = div
mulOpAsFunc (Mod _)   = mod
