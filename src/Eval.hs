module Eval where

import Data
import Data.IORef
import qualified Data.Map as M
import System.IO

data Variable = MethodVar [String] [Statement]
              | BlockVar [Statement]
              | BuiltinMethod ([Variable] -> IO Variable)
              | IntegerVar Int
              | BoolVar Bool
              | ConsVar Variable Variable
              | NoneVar

-- Make them showable for debugging
instance Show Variable where
	show (MethodVar args stmts) = "MethodVar " ++ show args ++ " " ++ show stmts
	show (BlockVar stmts) = "BlockVar " ++ show stmts
	show (BuiltinMethod x) = "BuiltinMethod"
	show (IntegerVar var) = "IntegerVar " ++ show var
	show (BoolVar var) = "BoolVar " ++ show var
	show (ConsVar a b) = "ConsVar " ++ show a ++ " " ++ show b
	show NoneVar = "NoneVar"

-- What the user gets when they print something.
to_str (MethodVar args stmts) = "MethodVar " ++ show args ++ " " ++ show stmts
to_str (BlockVar stmts) = "BlockVar " ++ show stmts
to_str (BuiltinMethod x) = "BuiltinMethod"
to_str (IntegerVar var) =  show var
to_str (BoolVar var) =  show var
to_str (ConsVar a b) = "(" ++ to_str a ++ " . " ++ to_str b ++ ")"
to_str NoneVar = "None"

-- Things are equal if they're obviously equal, otherwise false.
equal (IntegerVar x) (IntegerVar y) = x == y
equal (BoolVar x) (BoolVar y) = x == y
equal (NoneVar) (NoneVar) = True
equal (ConsVar aa ab) (ConsVar ba bb) = aa `equal` ba && ab `equal` bb
equal _ _ = False

greater (IntegerVar x) (IntegerVar y) = x > y
greater (BoolVar x) (BoolVar y) = x > y
greater (NoneVar) (NoneVar) = True -- To make comparing lists work.
greater (ConsVar aa ab) (ConsVar ba bb) = aa `greater` ba && ab `greater` bb
greater _ _ = False



-- The internal state of the interpreter.
type Variables = IORef (M.Map String Variable)

type Env = IORef (Variables, [(Bool, Variables)])

newEnv = do
	globals <- newIORef M.empty
	newIORef (globals, [])


-- Get a variable.
getLocalVar :: Variables -> String -> IO (Maybe Variable)
getLocalVar io_vars name = do
	vars <- readIORef io_vars
	return $ M.lookup name vars


-- Try to get a local variable, or get a global variable if it's not found.
getVar :: Env -> String -> IO Variable
getVar env name = do 
	(globalEnv, ((_,localEnv):_)) <- readIORef env
	local <- getLocalVar localEnv name
	case local of
		Just var -> return var
		Nothing -> do
			global <- getLocalVar globalEnv name
			case global of
				Just var -> return var
				Nothing -> error $ "Reference to unbound variable '" ++ name ++ "'"


-- Set a variable.
setVar :: Variables -> String -> Variable -> IO ()
setVar io_vars name value = modifyIORef io_vars $ M.insert name value 

-- Set a global variable
setGlobalVar :: Env -> String -> Variable -> IO ()
setGlobalVar env name value = do
	(globalEnv, _) <- readIORef env
	setVar globalEnv name value

-- Set a local variavle.
setLocalVar :: Env -> String -> Variable -> IO ()
setLocalVar env name value = do
	(_, ((_, localEnv):_)) <- readIORef env
	setVar localEnv name value

-- Push an empty frame onto the stack.
pushStack :: Env -> IO ()
pushStack env = do
	(globals, locals) <- readIORef env
	newLocals <- newIORef M.empty
	writeIORef env (globals, (False, newLocals):locals)

-- Discard a frame from the stack.
popStack :: Env -> IO ()
popStack env = do
	(globals, _:locals) <- readIORef env
	writeIORef env (globals, locals)

-- Get the flag.
getFlag :: Env -> IO Bool
getFlag env = do
	(_, ((flag, _):_)) <- readIORef env
	return flag

-- Set the flag.
setFlag :: Env -> Bool -> IO ()
setFlag env flag = do
	(globals, ((_, locals):stack)) <- readIORef env
	writeIORef env (globals, ((flag, locals):stack))


-- Evaluate an expression in env.
eval_exp :: Env -> Expression -> IO Variable
eval_exp env exp = 
	case exp of
		Identifier name -> getVar env name
		Add exprs -> (mapM (eval_exp env) exprs) >>= return . int_op (+)
		Sub exprs -> (mapM (eval_exp env) exprs) >>= return . int_op (-)
		Mul exprs -> (mapM (eval_exp env) exprs) >>= return . int_op (*)
		Div exprs -> (mapM (eval_exp env) exprs) >>= return . int_op div
		Call name args -> (mapM (eval_exp env) args) >>= call env name
		Cons a_exp b_exp -> do 
			a <- eval_exp env a_exp
			b <- eval_exp env b_exp
			return $ ConsVar a b
		Car exp -> do
			val <- eval_exp env exp
			return $ case val of
				ConsVar a b -> a
		Cdr exp -> do
			val <- eval_exp env exp
			return $ case val of
				ConsVar a b -> b


-- Make an operation from an integer function.
int_op :: (Int -> Int -> Int) -> [Variable] -> Variable
int_op op = foldl1 $ (\(IntegerVar a) (IntegerVar b) -> IntegerVar $ op a b)


-- Execute a statement, returning the value only if it returned.
exec_stmt :: Env -> Statement -> IO (Maybe Variable)
exec_stmt env stmt = case stmt of
	Return expr -> eval_exp env expr >>= return . Just
	ExprStatement expr -> eval_exp env expr >> return Nothing
	Conditional req_flag cond_stmt -> do
		flag <- getFlag env
		if flag == req_flag
			then
				exec_stmt env cond_stmt
			else
				return Nothing
	Compare name args -> do 
		_args <- (mapM (eval_exp env) args)
		retval <- call env name _args
		case retval of
			BoolVar flag -> setFlag env flag >> return Nothing
	Assign name exp -> eval_exp env exp >>= setLocalVar env name >> return Nothing
	RunBlock name -> do
		var <- getVar env name
		case var of
			BlockVar stmts -> run_block env stmts
	Write handle expr -> eval_exp env expr >>= hPutStrLn handle . to_str >> return Nothing


-- Call a method by name, returning the return value.
call :: Env -> String -> [Variable] -> IO Variable
call env name args = do
	meth <- getVar env name
	case meth of
		MethodVar arg_names stmts -> do
			pushStack env
			sequence $ zipWith (setLocalVar env) arg_names args
			retval <- get_retval env stmts
			popStack env
			return $ retval
		BuiltinMethod f -> f args

-- Get the return value when executing a block.
-- Defaults to NoneVar if it didn't return.
get_retval :: Env -> [Statement] -> IO Variable
get_retval env stmts = run_block env stmts >>= return . maybe NoneVar id

-- Run a block of statements, returning the value if it returned.
run_block :: Env -> [Statement] -> IO (Maybe Variable)
run_block env (statement:rest) = do
	retval <- exec_stmt env statement
	case retval of
		Just var -> return $ Just var
		Nothing -> run_block env rest
run_block env [] = return Nothing


