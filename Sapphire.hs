module Sapphire where
import Parser
import Lex
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import System.IO.Unsafe
import Data.IORef
import Data.Maybe
import qualified Data.Map as M


type Value = Double
type Weight = Double
type Connectome = M.Map (String,String) (TMVar Value, TVar Weight)

connectome:: IORef Connectome
connectome = unsafePerformIO $ newIORef M.empty 
neurons:: IORef (M.Map String (TVar Neural)) 
neurons = unsafePerformIO $ newIORef M.empty 

main = do 
	ast <- parser
	mapM grow ast
	nrns <- readIORef neurons
	finalneurons <- mapM (\x ->  atomically (readTVar x)) (map snd $ M.toList nrns)
	mapM_ spawn finalneurons
	return () 

parser = do 
	source <- readFile "test.nst"
	return $  parse (scanTokens source)




data Neural = Neuron	{ identity      :: String
			, inputs 	:: [String]
			, outputs 	:: [String]
		     	}  deriving(Show) 


spawn (Neuron i inp out) = forkIO $ forever $ do
			cnntm <- readIORef connectome 
			inputTmvars <- return $ catMaybes  $ map (\x -> M.lookup (x,i) cnntm) inp 
			outputTmvars <- return $  catMaybes $  map (\x -> M.lookup (i,x) cnntm) out 
			inputValues <- atomically $ forM inputTmvars $ \(tv,tw) -> do
					v <- takeTMVar tv
					w <- readTVar tw
					return (v,w)
			let output = 1 / (1 + (exp $ -1*(sum (map (\(x,x')-> x*x') inputValues))))
			atomically $ forM_ outputTmvars $ \(tv,tw) -> do 
					putTMVar tv output 
			putStrLn $ "!:" ++ i

select:: String -> IO (TVar Neural)
select name = do
        nrns <- readIORef neurons
        case M.lookup name nrns of
                Just neuron -> do
                        return neuron
                Nothing -> do
                        neuron <- atomically $ do
                                val <- newTVar 0
                                newTVar $ Neuron name [] []
                        modifyIORef neurons (M.insert name neuron)
                        return neuron


grow (Arrow' (Neuron' name) (Neuron' name')) = do
	tn <- select name
	tn' <- select name'
	n  <- atomically $ readTVar tn
	n' <- atomically $ readTVar tn'
	atomically $ do
		writeTVar tn  (Neuron (identity n)  (inputs n) (name':(outputs n)))
		writeTVar tn' (Neuron (identity n') (name:(inputs n')) (outputs n))
	synapse <- atomically$do
			v <-  newEmptyTMVar
			w <-  newTVar 1 --change
			return (v,w)
	modifyIORef connectome (M.insert (name,name') synapse) 

