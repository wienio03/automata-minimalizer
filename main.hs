module MinimalDfa where

----------------------------------------------------------------------------------------------
-- 1. Definicje typow
-- Zakladamy, ze stany to liczby calkowite (Integer) natomiast symbole to znaki (Char)
----------------------------------------------------------------------------------------------

type State = Int

type Symbol = Char

----------------------------------------------------------------------------------------------
-- 2. Funkcja przejscia, automaty DFA, NFA, ENFA
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

initialPartition :: DFA -> [[State]]
initialPartition dfa =
  filter (not . null) [acceptingStates, notAcceptingStates]
  where
    acceptingStates = dfaAcceptingStates dfa
    notAcceptingStates = filter (not . (`elem` acceptingStates)) (dfaStates dfa)

----------------------------------------------------------------------------------------------
-- Zwraca liste stanow, ktore pod wplywem symbolu c przechodza do stanow z listy a
----------------------------------------------------------------------------------------------

computeX :: DFA -> [State] -> Symbol -> [State]
computeX dfa a c =
  [s | s <- dfaStates dfa, let s' = dfaTransition dfa s c, s' `elem` a]

----------------------------------------------------------------------------------------------
-- Bierzemy zbior X z computeX i zwracamy (nowaPartycja, nowaWorklista) na podstawie
-- pojedynczego zbioru Y ze zbioru partycji jak w pseudokodzie
----------------------------------------------------------------------------------------------

splitClasses :: [State] -> [[State]] -> [[State]] -> [State] -> ([[State]], [[State]])
splitClasses x p w y =
  let yIntersectX = filter (`elem` x) y
      yDifferenceX = filter (not . (`elem` x)) y
   in if not (null yIntersectX) && not (null yDifferenceX)
        then
          let p' = p ++ [yIntersectX, yDifferenceX]

              isYinW = y `elem` w
              smallerSet = if length yIntersectX <= length yDifferenceX then yIntersectX else yDifferenceX
              biggerSet = if smallerSet == yIntersectX then yDifferenceX else yIntersectX

              w'
                | isYinW = removeOne y w ++ [yIntersectX, yDifferenceX]
                | otherwise = w ++ [smallerSet]
           in (p', w')
        else
          (p ++ [y], w)

----------------------------------------------------------------------------------------------
-- Helper do usuwania pierwszego wystąpienia elementu z listy
----------------------------------------------------------------------------------------------

removeOne :: (Eq a) => a -> [a] -> [a]
removeOne _ [] = []
removeOne x (h : t)
  | x == h = t
  | otherwise = h : removeOne x t

----------------------------------------------------------------------------------------------
-- Funkcja, ktora dla danego zbioru A z worklisty W, obecnej partycji P i worklisty W oraz
-- symbolu c oblicza X za pomoca computeX oraz dla kazdej klasy Y z partycji P
-- rozbija ja za pomoca splitClasses
----------------------------------------------------------------------------------------------
refine :: DFA -> [State] -> ([[State]], [[State]]) -> Symbol -> ([[State]], [[State]])
refine dfa a (p, w) c =
  let x = computeX dfa a c
      (p', w') = foldl (\(pAcc, wAcc) y -> let (pAcc', wAcc') = splitClasses x pAcc wAcc y in (pAcc', wAcc')) ([], w) p
   in (p', w')

----------------------------------------------------------------------------------------------
-- Glowna petla minimalizacji (odpowiednik while W != empty w pseudokodzie)
----------------------------------------------------------------------------------------------

hopcroftLoop :: DFA -> [[State]] -> [[State]] -> [[State]]
hopcroftLoop _ p [] = p
hopcroftLoop dfa p (a : w) =
  let (p', w') = foldl (refine dfa a) (p, w) (dfaAlphabet dfa)
   in hopcroftLoop dfa p' w'

----------------------------------------------------------------------------------------------
-- Funkcja, ktora buduje automat minimalny
----------------------------------------------------------------------------------------------

buildMinimizedDFA :: DFA -> [[State]] -> DFA
buildMinimizedDFA dfa finalPartition =
  let newStates = [0 .. length finalPartition - 1]

      mapOldStateToNew :: State -> State
      mapOldStateToNew s =
        case [i | (cls, i) <- zip finalPartition [0 ..], s `elem` cls] of
          (x : _) -> x
          [] -> error "blad w trakcie minimalizacji DFA, nie znaleziono stanu"

      newTransition :: State -> Symbol -> State
      newTransition i c =
        let oldRep = head (finalPartition !! i)
            oldDest = dfaTransition dfa oldRep c
         in mapOldStateToNew oldDest

      newStart = mapOldStateToNew (dfaStartState dfa)

      newAccept = [i | (cls, i) <- zip finalPartition [0 ..], any (`elem` dfaAcceptingStates dfa) cls]
   in DFA
        { dfaStates = newStates,
          dfaAlphabet = dfaAlphabet dfa,
          dfaTransition = newTransition,
          dfaStartState = newStart,
          dfaAcceptingStates = newAccept
        }

----------------------------------------------------------------------------------------------
-- Glowna funkcja minimalizujaca automat
----------------------------------------------------------------------------------------------

hopcroftMinimize :: DFA -> DFA
hopcroftMinimize dfa =
  let p0 = initialPartition dfa
      w0 = p0
      finalPartition = hopcroftLoop dfa p0 w0
   in buildMinimizedDFA dfa finalPartition

----------------------------------------------------------------------------------------------
-- Funkcja do wypisywania automatu (wystarczy wywolac w GHCI > printDFA <nazwa zmiennej>)
----------------------------------------------------------------------------------------------

printDFA :: DFA -> IO ()
printDFA dfa = do
  print (dfaAlphabet dfa)
  print (dfaStates dfa)

----------------------------------------------------------------------------------------------
-- Funkcja do wczytywania automatu z pliku
----------------------------------------------------------------------------------------------

processDFA :: DFA -> IO ()
processDFA dfa = do
  putStrLn "minimalizowanie DFA..."
  let minimized = hopcroftMinimize dfa
  printDFA minimized

processNFA :: NFA -> IO ()
processNFA nfa = do
  putStrLn "determinizowanie NFA..."
  let dfa = determinize nfa
  processDFA dfa

processENFA :: ENFA -> IO ()
processENFA enfa = do
  putStrLn "konwertowanie ENFA na NFA..."
  let nfa = eNfaToNfa enfa
  processNFA nfa

parseDFA :: [String] -> DFA
parseDFA (statesLine : alphabetLine : transitionsLines) =
  let states = read statesLine :: [State]
      alphabet = read alphabetLine :: [Symbol]
      transitions = map parseTransition (init $ init transitionsLines)
      startState = read $ last $ init transitionsLines
      acceptingStates = read $ last transitionsLines
   in DFA
        { dfaStates = states,
          dfaAlphabet = alphabet,
          dfaTransition = buildDeterministicTransition transitions,
          dfaStartState = startState,
          dfaAcceptingStates = acceptingStates
        }
  where
    parseTransition line =
      let [s, c, t] = words line
       in (read s, head c, read t)

    buildDeterministicTransition :: [(State, Symbol, State)] -> DeterministicTransition
    buildDeterministicTransition transitions s c =
      case [t | (s', c', t) <- transitions, s' == s, c' == c] of
        (t : _) -> t
        _ -> error $ "Nieznane przejście dla stanu " ++ show s ++ " i symbolu " ++ show c

parseNFA :: [String] -> NFA
parseNFA (statesLine : alphabetLine : transitionsLines) =
  let states = read statesLine :: [State]
      alphabet = read alphabetLine :: [Symbol]
      transitions = map parseTransition (init $ init transitionsLines)
      startState = read $ last $ init transitionsLines
      acceptingStates = read $ last transitionsLines
   in NFA
        { nfaStates = states,
          nfaAlphabet = alphabet,
          nfaTransition = buildNondeterministicTransition transitions,
          nfaStartState = startState,
          nfaAcceptingStates = acceptingStates
        }
  where
    parseTransition :: String -> (State, Maybe Symbol, [State])
    parseTransition line =
      let (s : c : ts) = words line -- ts to lista reszty slow
          symbol = if c == "ε" then Nothing else Just (head c)
       in (read s, symbol, map read ts)

    buildNondeterministicTransition :: [(State, Maybe Symbol, [State])] -> NondeterministicTransition
    buildNondeterministicTransition transitions s mc =
      case [ts | (s', c', ts) <- transitions, s' == s, c' == mc] of
        (ts : _) -> ts
        _ -> []

parseENFA :: [String] -> ENFA
parseENFA (statesLine : alphabetLine : transitionsLines) =
  let states = read statesLine :: [State]
      alphabet = read alphabetLine :: [Symbol]
      transitions = map parseTransition (init $ init transitionsLines)
      startState = read $ last $ init transitionsLines
      acceptingStates = read $ last transitionsLines
   in ENFA
        { eNfaStates = states,
          eNfaAlphabet = alphabet,
          eNfaTransition = buildNondeterministicTransition transitions,
          eNfaStartState = startState,
          eNfaAcceptingStates = acceptingStates
        }
  where
    parseTransition :: String -> (State, Maybe Symbol, [State])
    parseTransition line =
      let (s : c : ts) = words line
          symbol = if c == "ε" then Nothing else Just (head c)
       in (read s, symbol, map read ts)

    buildNondeterministicTransition :: [(State, Maybe Symbol, [State])] -> NondeterministicTransition
    buildNondeterministicTransition transitions s mc =
      case [ts | (s', c', ts) <- transitions, s' == s, c' == mc] of
        (ts : _) -> ts
        _ -> []

processAutomaton :: FilePath -> IO ()
processAutomaton path = do
  content <- lines <$> readFile path
  case content of
    ("DFA" : rest) -> processDFA $ parseDFA rest
    ("NFA" : rest) -> processNFA $ parseNFA rest
    ("ENFA" : rest) -> processENFA $ parseENFA rest
    _ -> putStrLn "Nieprawidłowy format pliku!"