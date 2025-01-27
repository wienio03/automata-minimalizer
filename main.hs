module Main where

import Algorithms
import DataTypes
import ExampleTests
import Parser
import Printer

----------------------------------------------------------------------------------------------
-- Glowne funkcje (procesowanie automatow do wyjscia, odpalenie przygotowanych testow,
-- funkcja gdzie podajemy nazwe pliku i dostajemy wyjscie z minimalnym DFA)
----------------------------------------------------------------------------------------------

mainTests :: IO ()
mainTests = run5TestCases

processAutomaton :: Either String (Either DFA (Either NFA ENFA)) -> IO ()
processAutomaton (Left err) = putStrLn $ "Error when processing the automaton" ++ err
processAutomaton (Right (Right (Right enfa))) = do
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
processAutomaton (Right (Right (Left nfa))) = do
  putStrLn "Parsed NFA:"
  printNFA nfa
  let dfa = determinize nfa
  putStrLn "\nConverted to DFA:"
  printDFA dfa
  let minDfa = hopcroftMinimize dfa
  putStrLn "\nMinimized DFA:"
  printDFA minDfa
processAutomaton (Right (Left dfa)) = do
  putStrLn "Parsed DFA:"
  printDFA dfa
  let minDfa = hopcroftMinimize dfa
  putStrLn "\nMinimized DFA:"
  printDFA minDfa

main :: IO ()
main = do
  putStrLn "Enter the filename of input file"
  fileName <- getLine
  content <- lines <$> readFile fileName
  let parsedAutomaton = parseAutomaton content
  processAutomaton parsedAutomaton