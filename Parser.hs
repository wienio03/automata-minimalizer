module Parser where

import DataTypes

----------------------------------------------------------------------------------------------
-- Funkcje do parsowania DFA, NFA, εNFA z pliku
----------------------------------------------------------------------------------------------

parseAutomaton :: [String] -> Either String (Either DFA (Either NFA ENFA))
parseAutomaton ("DFA" : rest) =
  case parseDFA rest of
    Just dfa -> Right (Left dfa)
    Nothing -> Left "Niepoprawny format DFA"
parseAutomaton ("NFA" : rest) =
  case parseNFA rest of
    Just nfa -> Right (Right (Left nfa))
    Nothing -> Left "Niepoprawny format NFA"
parseAutomaton ("ENFA" : rest) =
  case parseENFA rest of
    Just enfa -> Right (Right (Right enfa))
    Nothing -> Left "Niepoprawny format ENFA"
parseAutomaton _ = Left "Nieznany typ automatu"

parseDFA :: [String] -> Maybe DFA
parseDFA content = do
  (states, rest1) <- parseSection "STATES" readStates content
  (alphabet, rest2) <- parseSection "ALPHABET" readAlphabet rest1
  (transitions, rest3) <- parseSection "TRANSITIONS" readDfaTransitions rest2
  (startState, rest4) <- parseSection "STARTING STATE" readStartState rest3
  (acceptingStates, []) <- parseSection "ACCEPTING STATES" readStates rest4
  Just (DFA states alphabet (lookupTransition transitions) startState acceptingStates)

parseNFA :: [String] -> Maybe NFA
parseNFA content = do
  (states, rest1) <- parseSection "STATES" readStates content
  (alphabet, rest2) <- parseSection "ALPHABET" readAlphabet rest1
  (transitions, rest3) <- parseSection "TRANSITIONS" readNfaTransitions rest2
  (startState, rest4) <- parseSection "STARTING STATE" readStartState rest3
  (acceptingStates, []) <- parseSection "ACCEPTING STATES" readStates rest4
  Just (NFA states alphabet transitions startState acceptingStates)

parseENFA :: [String] -> Maybe ENFA
parseENFA content = do
  (states, rest1) <- parseSection "STATES" readStates content
  (alphabet, rest2) <- parseSection "ALPHABET" readAlphabet rest1
  (transitions, rest3) <- parseSection "TRANSITIONS" readEnfaTransitions rest2
  (startState, rest4) <- parseSection "STARTING STATE" readStartState rest3
  (acceptingStates, []) <- parseSection "ACCEPTING STATES" readStates rest4
  Just (ENFA states alphabet transitions startState acceptingStates)

parseSection :: String -> ([String] -> Maybe a) -> [String] -> Maybe (a, [String])
parseSection header parser (h : rest)
  | h == header = do
      let (sectionContent, rest') = span (\line -> line /= "STATES" && line /= "ALPHABET" && line /= "TRANSITIONS" && line /= "STARTING STATE" && line /= "ACCEPTING STATES") rest
      parsed <- parser sectionContent
      Just (parsed, rest')
  | otherwise = Nothing
parseSection _ _ _ = Nothing

readStates :: [String] -> Maybe [State]
readStates lines = Just (map read lines)

readAlphabet :: [String] -> Maybe [Symbol]
readAlphabet lines = Just (map head lines)

readStartState :: [String] -> Maybe State
readStartState [line] = Just (read line)
readStartState _ = Nothing

readDfaTransitions :: [String] -> Maybe [(State, Symbol, State)]
readDfaTransitions lines = Just [(read s1, head sym, read s2) | line <- lines, let [s1, sym, s2] = splitBy ',' line]

readNfaTransitions :: [String] -> Maybe NondeterministicTransition
readNfaTransitions lines = Just (\s sym -> [st | (s', sym', st) <- parsed, s' == s, sym' == sym])
  where
    parsed = [(read s1, if sym == "" then Nothing else Just (head sym), read s2) | line <- lines, let [s1, sym, s2] = splitBy ',' line]

readEnfaTransitions :: [String] -> Maybe NondeterministicTransition
readEnfaTransitions = readNfaTransitions

lookupTransition :: [(State, Symbol, State)] -> DeterministicTransition
lookupTransition trans s sym = case [(s', sym', s'') | (s', sym', s'') <- trans, s' == s, sym' == sym] of
  [(s', sym', s'')] -> s''
  _ -> error "Niepoprawne przejście dla DFA"

splitBy :: Char -> String -> [String]
splitBy _ "" = []
splitBy delimiter str =
  let (first, rest) = break (== delimiter) str
   in first : case rest of
        [] -> []
        (_ : remaining) -> splitBy delimiter remaining
