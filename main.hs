module MinimalDfa where

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
----------------------------------------------------------------------------------------------

----------------------------------------------------------------------------------------------
-- Definicje typow
-- Zakladamy, ze stany to liczby calkowite (Integer) natomiast symbole to znaki (Char)
----------------------------------------------------------------------------------------------

type State = Int

type Symbol = Char

----------------------------------------------------------------------------------------------
-- Funkcja przejscia, automaty DFA, NFA, ENFA
-- Zakladamy, ze funkcja przejscia bedzie miala sygnature State -> Symbol -> State
-- w DFA klasycznie funkcja przejscia ma sygnature δ: Q x Σ -> Q, wiec jest to poprawny opis
----------------------------------------------------------------------------------------------

type DeterministicTransition = State -> Symbol -> State

type NondeterministicTransition = State -> Maybe Symbol -> [State]

----------------------------------------------------------------------------------------------
-- DETERMINISTYCZNY AUTOMAT
-- dfaStates - lista stanow Q
-- dfaAlphabet - alfabet automatu Σ,
-- dfaTransition - funkcja przejscia (deterministyczna), sygnatura δ: Q x Σ -> Q
-- dfaStartState - poczatkowy stan automatu
-- dfaAcceptingStates - stany koncowe
----------------------------------------------------------------------------------------------

data DFA = DFA
  { dfaStates :: [State],
    dfaAlphabet :: [Symbol],
    dfaTransition :: DeterministicTransition,
    dfaStartState :: State,
    dfaAcceptingStates :: [State]
  }

----------------------------------------------------------------------------------------------
-- NIEDETERMINISTYCZNY AUTOMAT Z PUSTYMI PRZEJSCIAMI
-- dfaStates - lista stanow Q
-- dfaAlphabet - alfabet automatu Σ u ε,
-- dfaTransition - funkcja przejscia (niedeterministyczna), sygnatura δ: Q x (Σ u ε) -> 2^Q
-- dfaStartState - poczatkowy stan automatu
-- dfaAcceptingStates - stany koncowe
----------------------------------------------------------------------------------------------

data ENFA = ENFA
  { eNfaStates :: [State],
    eNfaAlphabet :: [Symbol],
    eNfaTransition :: NondeterministicTransition,
    eNfaStartState :: State,
    eNfaAcceptingStates :: [State]
  }

----------------------------------------------------------------------------------------------
-- NIEDETERMINISTYCZNY AUTOMAT
-- dfaStates - lista stanow Q
-- dfaAlphabet - alfabet automatu Σ,
-- dfaTransition - funkcja przejscia (niedeterministyczna), sygnatura δ: Q x Σ -> 2^Q
-- dfaStartState - poczatkowy stan automatu
-- dfaAcceptingStates - stany koncowe
----------------------------------------------------------------------------------------------

data NFA = NFA
  { nfaStates :: [State],
    nfaAlphabet :: [Symbol],
    nfaTransition :: NondeterministicTransition,
    nfaStartState :: State,
    nfaAcceptingStates :: [State]
  }

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

----------------------------------------------------------------------------------------------
-- Funkcje do wypisywania DFA, NFA, i εNFA
----------------------------------------------------------------------------------------------

printDFA :: DFA -> IO ()
printDFA (DFA states alphabet transition startState acceptingStates) = do
  putStrLn "DFA"
  putStrLn "STATES"
  mapM_ print states
  putStrLn "ALPHABET"
  mapM_ print alphabet
  putStrLn "TRANSITIONS"
  mapM_ (\(s, sym, s') -> putStrLn $ show s ++ "," ++ [sym] ++ "," ++ show s') [(s, sym, transition s sym) | s <- states, sym <- alphabet]
  putStrLn "STARTING STATE"
  print startState
  putStrLn "ACCEPTING STATES"
  mapM_ print acceptingStates

printNFA :: NFA -> IO ()
printNFA (NFA states alphabet transition startState acceptingStates) = do
  putStrLn "NFA"
  putStrLn "STATES"
  mapM_ print states
  putStrLn "ALPHABET"
  mapM_ print alphabet
  putStrLn "TRANSITIONS"
  mapM_ (\(s, sym) -> mapM_ (\s' -> putStrLn $ show s ++ "," ++ [sym] ++ "," ++ show s') (transition s (Just sym))) [(s, sym) | s <- states, sym <- alphabet]
  putStrLn "STARTING STATE"
  print startState
  putStrLn "ACCEPTING STATES"
  mapM_ print acceptingStates

printENFA :: ENFA -> IO ()
printENFA (ENFA states alphabet transition startState acceptingStates) = do
  putStrLn "ENFA"
  putStrLn "STATES"
  mapM_ print states
  putStrLn "ALPHABET"
  mapM_ print alphabet
  putStrLn "TRANSITIONS"
  mapM_ (\(s, sym) -> mapM_ (\s' -> putStrLn $ show s ++ "," ++ maybe "" (: []) sym ++ "," ++ show s') (transition s sym)) [(s, sym) | s <- states, sym <- map Just alphabet ++ [Nothing]]
  putStrLn "STARTING STATE"
  print startState
  putStrLn "ACCEPTING STATES"
  mapM_ print acceptingStates

----------------------------------------------------------------------------------------------
-- Usuwanie pustych przejsc (ENFA -> NFA)
-- δ̃(q,a) = εDOM(δ(εDOM(q),a))
----------------------------------------------------------------------------------------------

removeDuplicates :: (Eq a) => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x : xs) =
  if x `elem` xs
    then removeDuplicates xs
    else x : removeDuplicates xs

unionOnLists :: (Eq a) => [a] -> [a] -> [a]
unionOnLists xs [] = xs
unionOnLists xs (y : ys) =
  if y `elem` xs
    then unionOnLists xs ys
    else unionOnLists (xs ++ [y]) ys

unionAllLists :: (Eq a) => [[a]] -> [a]
unionAllLists [] = []
unionAllLists (l : ls) = unionOnLists l (unionAllLists ls)

epsilonClosure :: ENFA -> State -> [State]
epsilonClosure enfa s =
  let transition visited [] = visited
      transition visited (x : xs) =
        let epsilonReachable = eNfaTransition enfa x Nothing
            newStates = [st | st <- epsilonReachable, not (st `elem` visited)]
            visited' = visited ++ newStates
            xs' = xs ++ newStates
         in transition visited' xs'
   in transition [s] [s]

epsilonClosureSet :: ENFA -> [State] -> [State]
epsilonClosureSet enfa set = removeDuplicates (concatMap (epsilonClosure enfa) set)

eNfaToNfa :: ENFA -> NFA
eNfaToNfa enfa =
  let states = eNfaStates enfa
      alphabet = eNfaAlphabet enfa

      startState = eNfaStartState enfa
      acceptingStates = eNfaAcceptingStates enfa

      isAcceptingSet :: [State] -> Bool
      isAcceptingSet set = any (`elem` acceptingStates) set
      -- δ̃(q,a) = εDOM(δ(εDOM(q),a))
      newTransition :: State -> Maybe Symbol -> [State]
      newTransition s (Just c) =
        let ecl = epsilonClosure enfa s -- εDOM(q)
            transitionResult = [eNfaTransition enfa p (Just c) | p <- ecl] -- δ(εDOM(q),a)) jako lista list
            combined = unionAllLists transitionResult -- po polaczeniu w jedna liste bez duplikatow
            eclCombined = epsilonClosureSet enfa combined -- εDOM(δ(εDOM(q),a))
         in eclCombined
      newTransition _ Nothing = []

      newAcceptingStates :: [State]
      newAcceptingStates = [s | s <- states, isAcceptingSet (epsilonClosure enfa s)]

      newNFA =
        NFA
          { nfaStates = states,
            nfaAlphabet = alphabet,
            nfaTransition = newTransition,
            nfaStartState = startState,
            nfaAcceptingStates = newAcceptingStates
          }
   in newNFA

----------------------------------------------------------------------------------------------
-- Determinizacja (NFA -> DFA)
-- uzywam standardowego algorytmu determinizacji za pomoca konstrukcji podzbiorow, czyli
-- wynikowa funkcja przejscia ma tak naprawde sygnature δ: 2^Q x Σ -> 2^Q
----------------------------------------------------------------------------------------------

determinize :: NFA -> DFA
determinize nfa =
  let alphabet = nfaAlphabet nfa
      startSet = [nfaStartState nfa]

      transitionNFA :: State -> Symbol -> [State]
      transitionNFA s c = nfaTransition nfa s (Just c)

      isAcceptingSet :: [State] -> Bool
      isAcceptingSet set = any (`elem` nfaAcceptingStates nfa) set

      -- lista argumentow to: kolejka stanow do przetworzenia, juz przetworzone stany, istniejace przejscia
      -- zwracamy juz gotowe krotki ktore wykorzystamy do funkcji przejscia DFA
      buildDFA :: [[State]] -> [[State]] -> [([State], Symbol, [State])] -> ([[State]], [([State], Symbol, [State])])
      buildDFA [] visited edges = (visited, edges)
      buildDFA (s : qs) visited edges =
        let outTransitions = [(s, c, nextStates s c) | c <- alphabet]
            targets = [t | (_, _, t) <- outTransitions]

            newSets = [t | t <- targets, not (any (compareSets t) visited)]

            visited' = visited ++ newSets
            queue' = qs ++ newSets
            edges' = edges ++ outTransitions
         in buildDFA queue' visited' edges'

      nextStates :: [State] -> Symbol -> [State]
      nextStates set c =
        let listOfLists = [nfaTransition nfa s (Just c) | s <- set]
            combined = unionAllLists listOfLists
         in removeDuplicates combined

      (allSets, allEdges) = buildDFA [startSet] [startSet] []

      enumerate :: Int -> [[State]] -> [(Int, [State])]
      enumerate _ [] = []
      enumerate i (x : xs) = (i, x) : enumerate (i + 1) xs

      numbered :: [(Int, [State])]
      numbered = enumerate 0 allSets

      findIndexOf :: [State] -> Int
      findIndexOf set =
        case [i | (i, set') <- numbered, compareSets set' set] of
          (x : _) -> x
          [] -> error "blad w enumeracji w trakcie determinizacji, brak zbioru"

      newTransition :: State -> Symbol -> State
      newTransition i c =
        let oldSet = case lookupSet i numbered of
              Just s -> s
              _ -> error "brak stanu w lookupSet"
            possible =
              [ t | (s, sym, t) <- allEdges, compareSets s oldSet, sym == c
              ]
         in case possible of
              (res : _) -> findIndexOf res
              [] -> error "blad w trakcie determinizacji, brak przejścia (niemozliwe w pelnym NFA)"

      lookupSet :: Int -> [(Int, [State])] -> Maybe [State]
      lookupSet _ [] = Nothing
      lookupSet k ((i, s) : xs) =
        if i == k then Just s else lookupSet k xs

      newStates = [0 .. (length allSets - 1)]

      newStart = findIndexOf startSet

      newAccepting =
        [ i | (i, set) <- numbered, isAcceptingSet set
        ]

      newDFA =
        DFA
          { dfaStates = newStates,
            dfaAlphabet = alphabet,
            dfaTransition = newTransition,
            dfaStartState = newStart,
            dfaAcceptingStates = newAccepting
          }
   in newDFA

insertionSort :: (Ord a) => [a] -> [a]
insertionSort [] = []
insertionSort (x : xs) = insertToList x (insertionSort xs)
  where
    insertToList v [] = [v]
    insertToList v (w : ws) =
      if v <= w then v : w : ws else w : insertToList v ws

compareSets :: (Ord a) => [a] -> [a] -> Bool
compareSets s1 s2 =
  let s1' = insertionSort (removeDuplicates s1)
      s2' = insertionSort (removeDuplicates s2)
   in s1' == s2'

----------------------------------------------------------------------------------------------
-- Algorytm minimalizacji
-- Uzywam algorytmu minimalizacji DFA Hopcrofta z racji dobrej sredniej zlozonosci oraz
-- najlepszej pesymistycznej zlozonosci (rozwazalem jeszcze algorytm Brzozowskiego oraz Moore'a)
-- pseudokod:
----------------------------------------------------------------------------------------------
-- P := {F, Q \ F}
-- W := {F, Q \ F}
----------------------------------------------------------------------------------------------
-- while (W is not empty) do
-- choose and remove a set A from W
-- for each c in Σ do
-- let X be the set of states for which a transition on c leads to a state in A
-- for each set Y in P for which X ∩ Y is nonempty and Y \ X is nonempty do
-- replace Y in P by the two sets X ∩ Y and Y \ X
-- if Y is in W
-- replace Y in W by the same two sets
-- else
-- if |X ∩ Y| <= |Y \ X|
-- add X ∩ Y to W
-- else
-- add Y \ X to W:
----------------------------------------------------------------------------------------------

----------------------------------------------------------------------------------------------
-- Poczatkowa partycja na [Q\F, F]
----------------------------------------------------------------------------------------------

-- Funkcja tworząca początkową partycję na {F, Q \ F}
initialPartition :: DFA -> [[State]]
initialPartition dfa =
  [accepting, nonAccepting] >>= \p -> if null p then [] else [p]
  where
    accepting = dfaAcceptingStates dfa
    nonAccepting = filter (`notElem` accepting) (dfaStates dfa)

-- Funkcja znajdująca stany przechodzące na A pod wpływem symbolu c
findReachable :: DFA -> [State] -> Symbol -> [State]
findReachable dfa a c = [s | s <- dfaStates dfa, dfaTransition dfa s c `elem` a]

-- Podział klasy ekwiwalencji Y na dwie części zgodnie z X
splitClass :: [State] -> [State] -> ([State], [State])
splitClass x y = (filter (`elem` x) y, filter (`notElem` x) y)

-- Aktualizacja partycji zgodnie z algorytmem Hopcrofta
refinePartition :: DFA -> [[State]] -> [[State]] -> Symbol -> ([[State]], [[State]])
refinePartition dfa p w c = foldl process ([], w) p
  where
    process (newP, newW) y =
      let x = findReachable dfa y c
          (xIntY, yDiffX) = splitClass x y
       in case (xIntY, yDiffX) of
            ([], _) -> (newP ++ [y], newW)
            (_, []) -> (newP ++ [y], newW)
            _ ->
              let (smaller, larger) = if length xIntY <= length yDiffX then (xIntY, yDiffX) else (yDiffX, xIntY)
               in (newP ++ [xIntY, yDiffX], if y `elem` newW then remove y newW ++ [xIntY, yDiffX] else newW ++ [smaller])

-- Usuwanie pierwszego wystąpienia elementu z listy
remove :: (Eq a) => a -> [a] -> [a]
remove _ [] = []
remove x (y : ys)
  | x == y = ys
  | otherwise = y : remove x ys

-- Główna pętla algorytmu
hopcroftLoop :: DFA -> [[State]] -> [[State]] -> [[State]]
hopcroftLoop _ p [] = p
hopcroftLoop dfa p (a : w) =
  let (newP, newW) = foldl (\(pAcc, wAcc) c -> refinePartition dfa pAcc wAcc c) (p, w) (dfaAlphabet dfa)
   in hopcroftLoop dfa newP newW

-- Mapowanie starych stanów na nowe indeksy
stateMapping :: [[State]] -> State -> State
stateMapping partition s = case [i | (i, cls) <- zip [0 ..] partition, s `elem` cls] of
  (x : _) -> x
  [] -> error "State mapping failed"

-- Tworzenie minimalnego DFA
buildMinimizedDFA :: DFA -> [[State]] -> DFA
buildMinimizedDFA dfa partition =
  let newStates = [0 .. length partition - 1]
      newStart = stateMapping partition (dfaStartState dfa)
      newAccept = [i | (i, cls) <- zip newStates partition, any (`elem` dfaAcceptingStates dfa) cls]
      newTransition s c = stateMapping partition (dfaTransition dfa (head (partition !! s)) c)
   in DFA newStates (dfaAlphabet dfa) newTransition newStart newAccept

-- Minimalizacja DFA
hopcroftMinimize :: DFA -> DFA
hopcroftMinimize dfa = buildMinimizedDFA dfa (hopcroftLoop dfa (initialPartition dfa) (initialPartition dfa))

-- Wypisywanie przejść w DFA
printDFATransition :: DFA -> IO ()
printDFATransition dfa =
  mapM_ print [(q, a, dfaTransition dfa q a) | q <- dfaStates dfa, a <- dfaAlphabet dfa]

-- Wypisywanie przejść w NFA
printNFATransition :: NFA -> IO ()
printNFATransition nfa =
  mapM_ print [(q, a, nfaTransition nfa q (Just a)) | q <- nfaStates nfa, a <- nfaAlphabet nfa]

-- Wypisywanie przejść w ENFA
printENFATransition :: ENFA -> IO ()
printENFATransition enfa =
  mapM_ print [(q, a, eNfaTransition enfa q (Just a)) | q <- eNfaStates enfa, a <- eNfaAlphabet enfa]

main :: IO ()
main = do
  content <- lines <$> readFile "automaton.txt"
  case parseAutomaton content of
    Left err -> putStrLn err
    Right (Right (Right enfa)) -> do
      putStrLn "Parsed ENFA:"
      printENFA enfa
      let expectedEnfa =
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
      putStrLn "Expected ENFA:"
      printENFA expectedEnfa
      let nfa = eNfaToNfa enfa
      putStrLn "\nConverted to NFA:"
      printNFA nfa
      let dfa = determinize nfa
      putStrLn "\nConverted to DFA:"
      printDFA dfa
      let minDfa = hopcroftMinimize dfa
      putStrLn "\nMinimized DFA:"
      printDFA minDfa