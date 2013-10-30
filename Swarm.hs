module Swarm where
import Control.Concurrent.SampleVar
import Control.Concurrent
import Data.IORef
import Control.Monad


test query answer i j
	| i == (j+1)  = do return ()
	| otherwise =  do  
			feed query 1
        		val1<- probe answer
			print $ "answer"++(show i)++"-> "++ (show val1) 
			test query answer (i+1) j
main = do
        a<- cell 0
        b<- cell 1 
        fib<- cellEmpty
        query<- cellEmpty

        spawn2 a b  (\ x y -> do x + y) fib
	spawn2 b query (\ x y-> do x ) a
	spawn2 fib query (\ x y -> do x) b

        test query fib 0 20 

--------------------------------------------



data Cell a = Cell (SampleVar a) (IORef [SampleVar a])
cell a = do 
	value <- newSampleVar a
	empty <- newEmptySampleVar
	listeners <- newIORef [empty] 
	baby<-return $ Cell value listeners
	birth baby
	return baby
cellEmpty = do
        value <- newEmptySampleVar 
        empty <- newEmptySampleVar
        listeners <- newIORef [empty]
        baby<-return $ Cell value listeners
        birth baby
        return baby
harpoon (Cell val listeners) = do
	node <- newEmptySampleVar
	modifyIORef listeners(\x-> x ++ [node]) 
	return node
inject (Cell val listeners) = do
	return val
birth c = forkIO (alive c)
alive c@(Cell val listeners)= do 
	val <- readSampleVar val
	hearing <- readIORef listeners  
  	mapM (\x-> writeSampleVar x val) hearing
	forkIO (alive c)
	return ()
feed c@(Cell val listeners) value= do
	writeSampleVar val value
probe c@(Cell val listeners) = do 
	hearing <-(readIORef listeners)
	readSampleVar (hearing !! 0)
spawn1 inCell f outcell = do 
	c1 <- harpoon inCell
	(forkIO (propagator1 c1 f outcell))
propagator1 inCell f outCell = do 
        input <- readSampleVar inCell
        output <-return (f input)
        feed outCell output
        forkIO $ propagator1 inCell f outCell
        return ()
spawn2 inCell1 inCell2 f outCell = do
	c1 <- harpoon inCell1 
	c2 <- harpoon inCell2
	forkIO $ propagator2 c1 c2 f outCell
propagator2 inCell1 inCell2 f outCell = do
        input1 <- readSampleVar inCell1
        input2 <- readSampleVar inCell2
        output <-return (f input1 input2)
        feed  outCell output
        forkIO $ propagator2 inCell1 inCell2 f outCell
        return ()
spawn3 inCell1 inCell2 inCell3 f outCell = do
        c1 <- harpoon inCell1
        c2 <- harpoon inCell2
	c3 <- harpoon inCell3
        forkIO $ propagator3 c1 c2 c3 f outCell
propagator3 inCell1 inCell2 inCell3 f outCell = do
        input1 <- readSampleVar inCell1
        input2 <- readSampleVar inCell2
        input3 <- readSampleVar inCell3
        output <-return (f input1 input2 input3)
        feed outCell output
        forkIO $ (propagator3 inCell1 inCell2 inCell3 f outCell)
        return ()
 
