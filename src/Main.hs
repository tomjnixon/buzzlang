import Eval
import Data
import Parser
import System( getArgs )

main = do
	-- The name of the file to load is the first arg.
	[f_name] <- getArgs
	-- Parse the file.
	maybe_parsed <- parseProgramFile f_name
	-- Check if it parsed correctly.
	case maybe_parsed of
		Left err  -> print err
		Right parsed  -> do
			-- Set up the environment.
			env <- init_env parsed
			-- Call the main function.
			result <- eval_exp env (Call "main" [])
			-- Discard the result.
			return ()


-- Initialise an environment with a parse tree.
init_env parsed = do
	env <- newEnv
	pushStack env
	-- Add some constants.
	sequence $ [setGlobalVar env word (IntegerVar num) 
	            | (num, word) <- zip [0..10] $ words "zero one two three four five six seven eight nine ten"]
	-- Comparison functions.
	setGlobalVar env "equal" $ BuiltinMethod $ return . BoolVar . (\xs-> all (equal $ head xs) $ tail xs)
	setGlobalVar env "greater" $ BuiltinMethod $ return . BoolVar . and . (\xs-> zipWith greater xs $ tail xs)
	setGlobalVar env "nonep" $ BuiltinMethod $ return . BoolVar . (\xs-> all (equal NoneVar) $ xs)
	setGlobalVar env "none" $ NoneVar
	-- Load the methods from the parse tree.
	load_env env parsed
	return env


-- Load methods and blocks from a parse tree into an environment.
load_env env (part:rest) = do 
	case part of
		Method name args stmts -> setGlobalVar env name $ MethodVar args stmts
		Block name stmts -> setGlobalVar env name $ BlockVar stmts
	load_env env rest
load_env env [] = return ()
