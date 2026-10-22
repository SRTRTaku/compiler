import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List (singleton)

type S = String -- Sigma
type Q = Int -- State
type Q' = Set.Set Q
type Q'' = Set.Set Q'

-- Q -> [S -> [Q]]
--type Delta = [(Q,[(S,[Q])])] 
type Delta = Map.Map Q (Map.Map S Q')

type NFA = (Q', [S], Delta, Q, Q')
--type NFA = NFA {
--    qs :: [Q],
--    s :: [S],
--    delta :: Delta,
--    q0 :: Q,
--    f :: [Q]
--}

-- Q' -> [S -> Q']
--type Delta' = [(Q', [(S, Q')])]
type Delta' = Map.Map Q' (Map.Map S Q')

type DFA = (Q'', [S], Delta', Q', Q'')
--data DFA = DFA {
--    qs' :: [Q'],
--    s' :: [S],
--    delta' :: Delta',
--    q0' :: Q',
--    f' :: [Q']
--}

--
--
data Regex = Empty           -- empty set
           | Chr S           -- a
           | Cat Regex Regex -- r_1 r_2
           | Alt Regex Regex -- r_1 | r_2
           | Star Regex      -- r_1 *
           deriving Show

sigma :: [S]
sigma = map singleton ['a'..'z']

regexToNFA :: Regex -> NFA
regexToNFA Empty = (qs, sigma, delta, q0, f)
    where
    qs = Set.fromList [0, 1]
    delta = Map.empty
    q0 = 0
    f = Set.fromList [1]
regexToNFA (Chr x) = (qs, sigma, delta, q0, f)
    where
    qs = Set.fromList [0, 1]
    delta = Map.fromList [(0, Map.fromList [(x, Set.fromList [1])])]
    q0 = 0
    f = Set.fromList [1]
regexToNFA r = error "not yet implementes"


runDFA :: String -> DFA -> Bool
runDFA xs dfa@(qs', s', delta', q0', f') = q' `Set.member` f'
    where
    q' = runDelta' q0' (reverse xs)
    runDelta' :: Q' -> String -> Q'
    runDelta' q [] = q
    runDelta' q (x:xs) = runDelta' nextQ xs
        where
        d = delta' Map.! q
        nextQ
            | Map.member q delta' && Map.member [x] d = d Map.! [x]
            | otherwise = q

run :: Regex -> String -> Bool
run regex s = runDFA s . toDFA . regexToNFA $ regex

--
--

toDFA :: NFA -> DFA
toDFA (qs, s, delta, q0, f) = (qs', s, delta', q0', f')
    where
    q0' = cl delta $ Set.singleton q0
    (qs', delta') = subsets delta (Set.singleton q0', Set.empty, Map.empty)
    f' = Set.fromList [q | q <- Set.toList qs' , not . Set.null $ Set.intersection q f]

cl :: Delta -> Q' -> Q'
cl delta xs
    | xs' == xs = xs
    | otherwise = cl delta xs'
    where
    --step xs = Set.fromList $ concat [eTrans delta x |x <- Set.toList xs]
    --xs' = Set.union xs (step xs)
    xs' = Set.foldr (\x acc -> Set.union acc (eTrans delta x)) xs xs

eTrans :: Delta -> Q -> Q'
eTrans delta p
    | Map.member p delta && Map.member "" d
        = d Map.! ""
    | otherwise = Set.empty
    where
    d = delta Map.! p

subsets :: Delta -> (Q'', Q'', Delta') -> (Q'', Delta')
subsets delta (q1, q2, d)
    | Set.null q1 = (q2, d)
    | Set.null q = subsets delta (q1', q2, d) -- ??
    | otherwise = subsets delta $ addQ delta q (q1', q2, d)
    where
    q:xs = Set.toList q1
    q1' = Set.fromList xs
    


addQ :: Delta -> Q' -> (Q'', Q'', Delta') -> (Q'', Q'', Delta')
addQ delta q (q1, q2, d) = (q1', q2', d')
    where
    (q1', omega) = foldl (addS' delta q q2) (q1, Map.empty) sigma
    q2' = Set.insert q q2
    d' = Map.insert q omega d
addS' :: Delta -> Q' -> Q'' -> (Q'', Map.Map S Q') -> S -> (Q'', Map.Map S Q')
addS' delta q q2 (q1, omega) s = addS delta (q, s) (q1, q2, omega)

addS :: Delta -> (Q', S) -> (Q'', Q'', Map.Map S Q') -> (Q'', Map.Map S Q')
addS delta (q, s) (q1, q2, omega) = (q1', omega')
    where
    q' = delD delta q s
    q1' | (q' == q) || (q' `Set.member` q1) || (q' `Set.member` q2) = q1
        | otherwise = Set.insert q' q1
    omega' = Map.insert s q' omega

delD :: Delta -> Q' -> S -> Q'
delD delta q s = cl delta q''
    where
    q' = cl delta q 
    qs = map f $ Set.toList q'
    f p | Map.member p delta && Map.member s d = d Map.! s
        | otherwise = Set.empty
        where
        d = delta Map.! p
    q'' = Set.unions qs


--
--
regex1 :: Regex
regex1 = Chr "a" -- a

test1 :: Bool
test1 = run regex1 "xyz"
--test1 = runDFA "" . toDFA . regexToNFA $ regex1
