module ExampleTests where

import Algorithms
import DataTypes
import Parser
import Printer

----------------------------------------------------------------------------------------------
-- ###### WAZNE ######
-- OCZEKIWANY FORMAT PLIKU WEJSCIOWEGO:
-- <TYP AUTOMATU (ENFA, NFA, DFA>
-- SLOWO STATES
-- W NOWYCH LINIACH ETYKIETY STANOW (LICZBY NATURALNE)
-- SLOWO ALPHABET
-- W NOWYCH LINIACH SYMBOLE ALFABETU
-- SLOWO TRANSITIONS
-- W NOWYCH LINIACH NAPISY <stan>,<symbol>,<stan>
-- SLOWO STARTING STATE
-- POJEDYNCZA ETYKIETA STANU (LICZBA NATURALNA)
-- ACCEPTING STATES
-- W NOWYCH LINIACH ETYKIETY STANOW (LICZBY NATURALNE)
-- ####################

-- PRZYKLADY JAK ZAPISYWAC AUTOMATY W PLIKU I PRZYKLADOWE OUTPUTY WRAZ Z TESTAMI
-- PRZEWIDZIANE SA JAKO PLIKI t1-5.txt oraz t1-5output.txt
----------------------------------------------------------------------------------------------

-- ZMIENNE DLA TESTU 1

expectedEnfa1 :: ENFA
expectedEnfa1 =
  ENFA
    [0, 1, 2]
    ['a', 'b']
    ( \s c -> case (s, c) of
        (0, Nothing) -> [1]
        (1, Just 'a') -> [1, 2]
        (2, Just 'b') -> [0]
        _ -> []
    )
    0
    [2]

expectedNfa1 :: NFA
expectedNfa1 =
  NFA
    [0, 1, 2]
    ['a', 'b']
    ( \s c -> case (s, c) of
        (0, Just 'a') -> [1, 2]
        (1, Just 'a') -> [1, 2]
        (2, Just 'b') -> [0]
        _ -> []
    )
    0
    [2]

expectedDfa1 :: DFA
expectedDfa1 =
  DFA
    [0, 1, 2, 3]
    ['a', 'b']
    ( \s c -> case (s, c) of
        (0, 'a') -> 1
        (1, 'a') -> 2
        (2, 'b') -> 3
        (3, 'a') -> 2
        (3, 'b') -> 0
        _ -> error "Invalid transition"
    )
    0
    [2, 3]

expectedMinDfa1 :: DFA
expectedMinDfa1 =
  DFA
    [0, 1, 2]
    ['a', 'b']
    ( \s c -> case (s, c) of
        (0, 'a') -> 1
        (1, 'a') -> 2
        (2, 'b') -> 0
        _ -> error "Invalid transition"
    )
    0
    [2]

-- ZMIENNE DLA TESTU 2

expectedNfa2 :: NFA
expectedNfa2 =
  NFA
    [0, 1, 2]
    ['a', 'b']
    ( \s c -> case (s, c) of
        (0, Just 'a') -> [1, 2]
        (1, Just 'b') -> [2]
        (2, Just 'b') -> [0]
        _ -> []
    )
    0
    [2]

expectedDfa2 :: DFA
expectedDfa2 =
  DFA
    [0, 1, 2, 3]
    ['a', 'b']
    ( \s c -> case (s, c) of
        (0, 'a') -> 1
        (1, 'b') -> 2
        (2, 'b') -> 3
        (3, 'a') -> 1
        _ -> error "Invalid transition"
    )
    0
    [2, 3]

expectedMinDfa2 :: DFA
expectedMinDfa2 =
  DFA
    [0, 1, 2]
    ['a', 'b']
    ( \s c -> case (s, c) of
        (0, 'a') -> 1
        (1, 'b') -> 2
        (2, 'b') -> 0
        _ -> error "Invalid transition"
    )
    0
    [2]

-- ZMIENNE DLA TESTU 3

expectedDfa3 :: DFA
expectedDfa3 =
  DFA
    [0, 1, 2]
    ['a', 'b']
    ( \s c -> case (s, c) of
        (0, 'a') -> 1
        (0, 'b') -> 2
        (1, 'a') -> 0
        (1, 'b') -> 2
        (2, 'a') -> 1
        (2, 'b') -> 0
        _ -> error "Invalid transition"
    )
    0
    [2]

expectedMinDfa3 :: DFA
expectedMinDfa3 = expectedDfa3

-- ZMIENNE DLA TESTU 4

expectedEnfa4 :: ENFA
expectedEnfa4 =
  ENFA
    [0, 1, 2, 3, 4]
    ['x', 'y', 'z']
    ( \s c -> case (s, c) of
        (0, Nothing) -> [1]
        (1, Just 'x') -> [2]
        (1, Just 'y') -> [3]
        (2, Just 'z') -> [4]
        (3, Just 'x') -> [4]
        (4, Just 'y') -> [0]
        _ -> []
    )
    0
    [4]

expectedNfa4 :: NFA
expectedNfa4 =
  NFA
    [0, 1, 2, 3, 4]
    ['x', 'y', 'z']
    ( \s c -> case (s, c) of
        (0, Just 'x') -> [2]
        (0, Just 'y') -> [3]
        (1, Just 'x') -> [2]
        (1, Just 'y') -> [3]
        (2, Just 'z') -> [4]
        (3, Just 'x') -> [4]
        (4, Just 'y') -> [0]
        _ -> []
    )
    0
    [4]

expectedDfa4 :: DFA
expectedDfa4 =
  DFA
    [0, 1, 2, 3, 4, 5, 6]
    ['x', 'y', 'z']
    ( \s c -> case (s, c) of
        (0, 'x') -> 1
        (0, 'y') -> 2
        (1, 'z') -> 3
        (2, 'x') -> 3
        (3, 'y') -> 4
        (4, 'x') -> 5
        (5, 'z') -> 6
        (6, 'y') -> 0
        _ -> error "Invalid transition"
    )
    0
    [4, 6]

expectedMinDfa4 :: DFA
expectedMinDfa4 =
  DFA
    [0, 1, 2, 3]
    ['x', 'y', 'z']
    ( \s c -> case (s, c) of
        (0, 'x') -> 1
        (0, 'y') -> 2
        (1, 'z') -> 3
        (2, 'x') -> 3
        (3, 'y') -> 0
        _ -> error "Invalid transition"
    )
    0
    [3]

-- ZMIENNE DLA TESTU 5

expectedNfa5 :: NFA
expectedNfa5 =
  NFA
    [0, 1, 2, 3, 4, 5]
    ['a', 'b', 'c', 'd']
    ( \s c -> case (s, c) of
        (0, Just 'a') -> [1]
        (0, Just 'b') -> [2]
        (1, Just 'c') -> [3]
        (2, Just 'c') -> [3]
        (3, Just 'd') -> [4]
        (4, Just 'a') -> [5]
        (5, Just 'b') -> [0]
        _ -> []
    )
    0
    [5]

expectedDfa5 :: DFA
expectedDfa5 =
  DFA
    [0, 1, 2, 3, 4, 5, 6]
    ['a', 'b', 'c', 'd']
    ( \s c -> case (s, c) of
        (0, 'a') -> 1
        (0, 'b') -> 2
        (1, 'c') -> 3
        (2, 'c') -> 3
        (3, 'd') -> 4
        (4, 'a') -> 5
        (5, 'b') -> 0
        _ -> error "Invalid transition"
    )
    0
    [5]

expectedMinDfa5 :: DFA
expectedMinDfa5 =
  DFA
    [0, 1, 2, 3, 4]
    ['a', 'b', 'c', 'd']
    ( \s c -> case (s, c) of
        (0, 'a') -> 1
        (0, 'b') -> 2
        (1, 'c') -> 3
        (2, 'c') -> 3
        (3, 'd') -> 4
        (4, 'a') -> 0
        _ -> error "Invalid transition"
    )
    0
    [4]

----------------------------------------------------------------------------------------------
-- FUNKCJE GLOWNE (DO TESTOWANIA PROGRAMU!)
----------------------------------------------------------------------------------------------

-- TA FUNCKJA JEST DO TESTOWANIA PRZYGOTOWANYCH 5 TESTOW DLA PROJEKTU (WYNIKOWE AUTOMATY SA W PLIKACH t1-5output.txt)
runTest :: String -> Either String (Either DFA (Either NFA ENFA)) -> IO ()
runTest name result = do
  putStrLn $ "== TEST CASE: " ++ name ++ " =="
  case result of
    Left err -> putStrLn $ "Błąd parsowania: " ++ err
    Right (Right (Right enfa)) -> do
      putStrLn "Parsed ENFA:"
      printENFA enfa
      let nfa = eNfaToNfa enfa
      putStrLn "\nConverted to NFA:"
      printNFA nfa
      let dfa = determinize nfa
      putStrLn "\nConverted to DFA:"
      printDFA dfa
      let minDfa = hopcroftMinimize dfa
      putStrLn "\nMinimized DFA:"
      printDFA minDfa
    Right (Right (Left nfa)) -> do
      putStrLn "Parsed NFA:"
      printNFA nfa
      let dfa = determinize nfa
      putStrLn "\nConverted to DFA:"
      printDFA dfa
      let minDfa = hopcroftMinimize dfa
      putStrLn "\nMinimized DFA:"
      printDFA minDfa
    Right (Left dfa) -> do
      putStrLn "Parsed DFA:"
      printDFA dfa
      let minDfa = hopcroftMinimize dfa
      putStrLn "\nMinimized DFA:"
      printDFA minDfa
  putStrLn "==============================\n"

run5TestCases :: IO ()
run5TestCases = do
  t1 <- parseAutomaton . lines <$> readFile "t1.txt"
  t2 <- parseAutomaton . lines <$> readFile "t2.txt"
  t3 <- parseAutomaton . lines <$> readFile "t3.txt"
  t4 <- parseAutomaton . lines <$> readFile "t4.txt"
  t5 <- parseAutomaton . lines <$> readFile "t5.txt"

  runTest "Test 1" t1
  runTest "Test 2" t2
  runTest "Test 3" t3
  runTest "Test 4" t4
  runTest "Test 5" t5
