module MinimalDfa where

----------------------------------------------------------------------------------------------
-- 1. Definicje typow
-- Zakladamy, ze stany to liczby calkowite (Integer) natomiast symbole to znaki (Char)
----------------------------------------------------------------------------------------------

type State = Int

type Symbol = Char

----------------------------------------------------------------------------------------------
-- 2. Funkcja przejscia
-- Zakladamy, ze funkcja przejscia bedzie miala sygnature State -> Symbol -> State
-- w DFA klasycznie funkcja przejscia ma sygnature Q x E -> Q, wiec jest to poprawny opis
----------------------------------------------------------------------------------------------

type Transition = State -> Symbol -> State

data DFA = DFA
  { dfaStates :: [State],
    dfaAlphabet :: [Symbol],
    dfaTransition :: Transition,
    dfaStartState :: State,
    dfaAcceptingStates :: [State]
  }

----------------------------------------------------------------------------------------------
-- 3. Algorytm minimalizacji
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
-- 4. Poczatkowa partycja na [Q\F, F]
----------------------------------------------------------------------------------------------
initialPartition :: DFA -> [[State]]
initialPartition dfa =
  filter (not . null) [acceptingStates, notAcceptingStates]
  where
    acceptingStates = dfaAcceptingStates dfa
    notAcceptingStates = filter (not . (`elem` acceptingStates)) (dfaStates dfa)

----------------------------------------------------------------------------------------------
-- 5. Zwraca liste stanow, ktore pod wplywem symbolu c przechodza do stanow z listy a
----------------------------------------------------------------------------------------------
computeX :: DFA -> [State] -> Symbol -> [State]
computeX dfa a c =
  [s | s <- dfaStates dfa, let s' = dfaTransition dfa s c, s' `elem` a]

----------------------------------------------------------------------------------------------
-- 6. Bierzemy zbior X z computeX i zwracamy (nowaPartycja, nowaWorklista) na podstawie
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
-- 7. Helper do usuwania pierwszego wystąpienia elementu z listy
----------------------------------------------------------------------------------------------

removeOne :: (Eq a) => a -> [a] -> [a]
removeOne _ [] = []
removeOne x (h : t)
  | x == h = t
  | otherwise = h : removeOne x t

----------------------------------------------------------------------------------------------
-- 8. Funkcja, ktora dla danego zbioru A z worklisty W, obecnej partycji P i worklisty W oraz
-- symbolu c oblicza X za pomoca computeX oraz dla kazdej klasy Y z partycji P
-- rozbija ja za pomoca splitClasses
----------------------------------------------------------------------------------------------

refine :: DFA -> [State] -> ([[State]], [[State]]) -> Symbol -> ([[State]], [[State]])
refine dfa a (p, w) c =
  let x = computeX dfa a c
      (p', w') = foldl (\(pAcc, wAcc) y -> let (pAcc', wAcc') = splitClasses x pAcc wAcc y in (pAcc', wAcc')) ([], w) p
   in (p', w')

----------------------------------------------------------------------------------------------
-- 9. Glowna petla minimalizacji (odpowiednik while W != empty w pseudokodzie)
----------------------------------------------------------------------------------------------

hopcroftLoop :: DFA -> [[State]] -> [[State]] -> [[State]]
hopcroftLoop _ p [] = p
hopcroftLoop dfa p (a : w) =
  let (p', w') = foldl (refine dfa a) (p, w) (dfaAlphabet dfa)
   in hopcroftLoop dfa p' w'

----------------------------------------------------------------------------------------------
-- 10. Funkcja, ktora buduje automat minimalny
----------------------------------------------------------------------------------------------

buildMinimizedDFA :: DFA -> [[State]] -> DFA
buildMinimizedDFA dfa finalPartition =
  let newStates = [0 .. length finalPartition - 1]

      mapOldStateToNew :: State -> State
      mapOldStateToNew s =
        case [i | (cls, i) <- zip finalPartition [0 ..], s `elem` cls] of
          (x : _) -> x
          [] -> error "Stan nieznaleziony - blad."

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
-- 11. Glowna funkcja minimalizujaca automat
----------------------------------------------------------------------------------------------

hopcroftMinimize :: DFA -> DFA
hopcroftMinimize dfa =
  let p0 = initialPartition dfa
      w0 = p0
      finalPartition = hopcroftLoop dfa p0 w0
   in buildMinimizedDFA dfa finalPartition

--------------------------------------------------------------------------------
-- PROSTY TEST
--------------------------------------------------------------------------------
transitionExample :: State -> Symbol -> State
transitionExample 0 'a' = 1
transitionExample 0 'b' = 2
transitionExample 1 'a' = 3
transitionExample 1 'b' = 4
transitionExample 2 'a' = 5
transitionExample 2 'b' = 4
transitionExample 3 'a' = 3
transitionExample 3 'b' = 5
transitionExample 4 'a' = 5
transitionExample 4 'b' = 4
transitionExample 5 'a' = 5
transitionExample 5 'b' = 5
transitionExample _ _ = error "Nieoczekiwany stan lub symbol!"

exampleDFA :: DFA
exampleDFA =
  DFA
    { dfaStates = [0, 1, 2, 3, 4, 5],
      dfaAlphabet = ['a', 'b'],
      dfaTransition = transitionExample,
      dfaStartState = 0,
      dfaAcceptingStates = [3, 4]
    }