module DataTypes where

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
