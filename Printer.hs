module Printer where

import DataTypes

----------------------------------------------------------------------------------------------
-- Funkcje do wypisywania DFA, NFA, i εNFA
----------------------------------------------------------------------------------------------

printDFA :: DFA -> IO ()
printDFA (DFA states alphabet transition startState acceptingStates) = do
  putStrLn "DFA:"
  putStrLn $ "  States: {" ++ unwords (map show states) ++ "}"
  putStrLn $ "  Alphabet: {" ++ unwords (map (: []) alphabet) ++ "}"
  putStrLn "  Transitions:"
  mapM_ (\(s, sym) -> putStrLn $ "    " ++ show s ++ " -> " ++ [sym] ++ " -> " ++ show (transition s sym)) [(s, sym) | s <- states, sym <- alphabet]
  putStrLn $ "  Start State: " ++ show startState
  putStrLn $ "  Accepting States: {" ++ unwords (map show acceptingStates) ++ "}"

printNFA :: NFA -> IO ()
printNFA (NFA states alphabet transition startState acceptingStates) = do
  putStrLn "NFA:"
  putStrLn $ "  States: {" ++ unwords (map show states) ++ "}"
  putStrLn $ "  Alphabet: {" ++ unwords (map (: []) alphabet) ++ "}"
  putStrLn "  Transitions:"
  mapM_ (\(s, sym) -> putStrLn $ "    " ++ show s ++ " -> " ++ [sym] ++ " -> {" ++ unwords (map show (transition s (Just sym))) ++ "}") [(s, sym) | s <- states, sym <- alphabet]
  putStrLn $ "  Start State: " ++ show startState
  putStrLn $ "  Accepting States: {" ++ unwords (map show acceptingStates) ++ "}"

printENFA :: ENFA -> IO ()
printENFA (ENFA states alphabet transition startState acceptingStates) = do
  putStrLn "ENFA:"
  putStrLn $ "  States: {" ++ unwords (map show states) ++ "}"
  putStrLn $ "  Alphabet: {" ++ unwords (map (: []) alphabet) ++ "}"
  putStrLn "  Transitions:"
  mapM_ (\(s, sym) -> putStrLn $ "    " ++ show s ++ " -> " ++ maybe "ε" (: []) sym ++ " -> {" ++ unwords (map show (transition s sym)) ++ "}") [(s, sym) | s <- states, sym <- map Just alphabet ++ [Nothing]]
  putStrLn $ "  Start State: " ++ show startState
  putStrLn $ "  Accepting States: {" ++ unwords (map show acceptingStates) ++ "}"
