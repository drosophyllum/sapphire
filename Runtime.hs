module Runtime where
import Parser
import Lex
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import System.IO.Unsafe
import Data.IORef
import qualified Data.Map as M

namespace :: IORef (M.Map String (TVar Neural))
namespace = unsafePerformIO $ newIORef $ M.empty

work :: [TVar Neural]

main = do 
	ast <- parser
	mapM grow ast
	return () 

parser = do 
	source <- readFile "test.nst"
	return $  parse (scanTokens source)


data Neural = Neuron	{ identity      :: String
			, value 	:: TVar Double
			, inputs 	:: [((Double,(TVar Neural)),Bool)]
			, outputs 	:: [TVar Neural]
		     	}   

grow (Arrow' (Neuron' name) (Neuron' name')) = do
	n <- select name
	n'<- select name'
	connect n n'

select:: String -> IO (TVar Neural)
select name = do
	nmspc <- readIORef namespace
	case M.lookup name nmspc of
		Just neuron -> do
			return neuron
		Nothing -> do 
			neuron <- atomically $ do
				val <- newTVar 0
				newTVar $ Neuron name val  [] []	
			modifyIORef namespace (M.insert name neuron) 
			return neuron

connect:: TVar Neural -> TVar Neural -> IO ()
connect neuron neuron' =  atomically $do
		(Neuron i v inp out) <- readTVar neuron
		(Neuron i v inp out) <- readTVar neuron'
		writeTVar neuron (Neuron i v inp (neuron':out))		 
		writeTVar neuron' (Neuron i v (((0,neuron),False):inp) out)


inject:: TVar Neural -> Val -> IO ()
inject neuron v =  atomically $ do
		(Neuron i v inp out) <- readTVar neuron 
		writeTVar neuron (Neuron i v inp out')
		where out' = map announce out
		

