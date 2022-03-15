module Main where

import System.Random
import Control.Monad (when)
import Control.Concurrent
import Control.Concurrent.STM


type Fork = TMVar Int

makeFork :: STM Fork
makeFork = newTMVar 1

pickUpFork :: Fork -> STM Int
pickUpFork f = takeTMVar f

putDownFork :: Fork -> STM ()
putDownFork f = putTMVar f 1


philosophers :: [String]
philosophers = ["Musk", "Crowder", "Jones", "Rogan", "Shapiro"]

dinePhils :: String -> Fork -> Fork -> IO ()
dinePhils name leftFork rightFork = do
    runLoop
        where
            runLoop = do
            putStrLn(name ++ " is hungry")
            atomically $ do
                pickUpFork leftFork
                pickUpFork rightFork
                return ()
            putStrLn(name ++ " is eating")
            delay <- randomRIO (1, 3)
            threadDelay (delay * 1000000)
            atomically $ do
                putDownFork leftFork
                putDownFork rightFork
            putStrLn (name ++ " is thinking")
            delay <- randomRIO (1, 3)
            threadDelay (delay * 1000000)
            runLoop

main = do
    fork1 <- atomically makeFork 
    fork2 <- atomically makeFork 
    fork3 <- atomically makeFork 
    fork4 <- atomically makeFork 
    fork5 <- atomically makeFork 
    finished <- newEmptyMVar
    forkIO (dinePhils (philosophers!!0) fork1 fork2 >> putMVar finished())
    forkIO (dinePhils (philosophers!!1) fork2 fork3 >> putMVar finished())
    forkIO (dinePhils (philosophers!!2) fork3 fork4 >> putMVar finished())
    forkIO (dinePhils (philosophers!!3) fork4 fork5 >> putMVar finished())
    forkIO (dinePhils (philosophers!!4) fork5 fork1 >> putMVar finished())
    takeMVar finished
    takeMVar finished
    takeMVar finished
    takeMVar finished
    takeMVar finished
    