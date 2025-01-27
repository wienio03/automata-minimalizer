module Algorithms where

import DataTypes

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
-- Używam standardowego algorytmu determinizacji za pomocą konstrukcji podzbiorów, czyli
-- wynikowa funkcja przejść ma tak naprawdę sygnaturę δ: 2^Q x Σ -> 2^Q
----------------------------------------------------------------------------------------------

determinize :: NFA -> DFA
determinize nfa =
  let alphabet = nfaAlphabet nfa
      startSet = [nfaStartState nfa]

      transitionNFA :: State -> Symbol -> [State]
      transitionNFA s c = nfaTransition nfa s (Just c)

      isAcceptingSet :: [State] -> Bool
      isAcceptingSet set = any (`elem` nfaAcceptingStates nfa) set

      -- Lista argumentów to: kolejka stanów do przetworzenia, już przetworzone stany, istniejące przejścia
      -- Zwracamy już gotowe krotki, które wykorzystamy do funkcji przejścia DFA
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
          [] -> error "Error when enumerating during determinization"

      newTransition :: State -> Symbol -> State
      newTransition i c =
        let oldSet = case lookupSet i numbered of
              Just s -> s
              _ -> error "State not found in lookup set"
            possible =
              [ t | (s, sym, t) <- allEdges, compareSets s oldSet, sym == c
              ]
         in case possible of
              (res : _) -> findIndexOf res
              [] -> error "Error during determinization (no transition)"

      lookupSet :: Int -> [(Int, [State])] -> Maybe [State]
      lookupSet _ [] = Nothing
      lookupSet k ((i, s) : xs) =
        if i == k then Just s else lookupSet k xs

      newStates = [0 .. (length allSets - 1)]

      newStart = findIndexOf startSet

      newAccept =
        [ i | (i, set) <- zip newStates allSets, isAcceptingSet set
        ]

      newDFA =
        DFA
          { dfaStates = newStates,
            dfaAlphabet = alphabet,
            dfaTransition = newTransition,
            dfaStartState = newStart,
            dfaAcceptingStates = newAccept
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
-- Używam algorytmu minimalizacji DFA Hopcrofta z racji dobrej średniej złożoności oraz
-- najlepszej pesymistycznej złożoności (rozważałem jeszcze algorytm Brzozowskiego oraz Moore'a)
-- Pseudokod:
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
-- Początkowa partycja na [Q\F, F]
----------------------------------------------------------------------------------------------

-- Funkcja tworząca początkową partycję na {F, Q \ F}
initialPartition :: DFA -> [[State]]
initialPartition dfa =
  let accepting = dfaAcceptingStates dfa
      nonAccepting = filter (`notElem` accepting) (dfaStates dfa)
   in filter (not . null) [accepting, nonAccepting]

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

-- Reordering function to ensure the partition containing the start state is first
reorderPartitionWithStartFirst :: DFA -> [[State]] -> [[State]]
reorderPartitionWithStartFirst dfa partition =
  case [cls | cls <- partition, dfaStartState dfa `elem` cls] of
    (startClass : _) -> startClass : filter (/= startClass) partition
    [] -> partition

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
  let reorderedPartition = reorderPartitionWithStartFirst dfa partition
      newStates = [0 .. length reorderedPartition - 1]
      newStart = dfaStartState dfa
      newAccept = [i | (i, cls) <- zip newStates reorderedPartition, any (`elem` dfaAcceptingStates dfa) cls]
      newTransition s c = stateMapping reorderedPartition (dfaTransition dfa (head (reorderedPartition !! s)) c)
   in DFA
        { dfaStates = newStates,
          dfaAlphabet = dfaAlphabet dfa,
          dfaTransition = newTransition,
          dfaStartState = newStart,
          dfaAcceptingStates = newAccept
        }

-- Minimalizacja DFA przy użyciu algorytmu Hopcrofta
hopcroftMinimize :: DFA -> DFA
hopcroftMinimize dfa =
  let initialP = initialPartition dfa
      initialW = initialPartition dfa
      finalPartition = hopcroftLoop dfa initialP initialW
   in buildMinimizedDFA dfa finalPartition
