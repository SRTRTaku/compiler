import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List (singleton, mapAccumL)

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
sigma = map singleton $ ['a'..'z'] ++ ['A'..'Z'] ++ ['@', '.']

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
regexToNFA (Cat r1 r2) = (qs, sigma, delta, q0, f)
    where
    nfa1 = regexToNFA r1
    nfa2 = regexToNFA r2
    (qn0, (qs1, sigma, delta1, q01, f1)) = reallocation initialState nfa1
    (qn0', (qs2, _, delta2, q02, f2)) = reallocation qn0 nfa2
    (p, qn0'') = stateSupply qn0'
    (q, _) = stateSupply qn0''
    qs = Set.insert p
       . Set.insert q
       $ Set.union qs1 qs2
    delta = Map.insert q12 (Map.fromList [("", Set.singleton q)])
          . Map.insert q11 (Map.fromList [("", Set.singleton q02)])
          . Map.insert p (Map.fromList [("", Set.singleton q01)])
          $ Map.union delta1 delta2
    q11 = Set.elemAt 0 f1
    q12 = Set.elemAt 0 f2
    q0 = p
    f = Set.singleton q
regexToNFA (Alt r1 r2) = (qs, sigma, delta, q0, f)
    where
    nfa1 = regexToNFA r1
    nfa2 = regexToNFA r2
    (qn0, (qs1, sigma, delta1, q01, f1)) = reallocation initialState nfa1
    (qn0', (qs2, _, delta2, q02, f2)) = reallocation qn0 nfa2
    (p, qn0'') = stateSupply qn0'
    (q, _) = stateSupply qn0''
    qs = Set.insert p
       . Set.insert q
       $ Set.union qs1 qs2
    delta = Map.insert q12 (Map.fromList [("", Set.singleton q)])
          . Map.insert q11 (Map.fromList [("", Set.singleton q)])
          . Map.insert p (Map.fromList [("", Set.fromList [q01, q02])])
          $ Map.union delta1 delta2
    q11 = Set.elemAt 0 f1
    q12 = Set.elemAt 0 f2
    q0 = p
    f = Set.singleton q
regexToNFA (Star r1) = (qs, sigma, delta, q0, f)
    where
    nfa1 = regexToNFA r1
    (qn0, (qs1, sigma, delta1, q01, f1)) = reallocation initialState nfa1
    (p, qn0') = stateSupply qn0
    (q, _) = stateSupply qn0'
    qs = Set.insert p
       . Set.insert q
       $ qs1
    delta = Map.insert q11 (Map.fromList [("", Set.fromList [q01, q])])
          . Map.insert p (Map.fromList [("", Set.fromList [q01, q])])
          $ delta1
    q11 = Set.elemAt 0 f1
    q0 = p
    f = Set.singleton q

reallocation :: SS -> NFA -> (SS, NFA)
reallocation qn0 (qs, s, delta, q0, f)
    = (qn0', (qs', s, delta', q0', f'))
    where
    (qn0', mp) =  mkMap qn0 (Set.toList qs)
    qs' = substituteSet mp qs
    delta' = substituteDelta mp delta
    q0' = sb mp q0
    f' = substituteSet mp f


substituteSet :: [(Q, Q)] -> Q' -> Q'
substituteSet mp qs = Set.map (sb mp) qs

substituteDelta :: [(Q, Q)] -> Delta -> Delta
substituteDelta mp delta = delta''
    where
    delta' = Map.mapKeys (sb mp) delta
    delta'' = Map.map (Map.map (Set.map (sb mp))) delta'

sb :: [(Q, Q)] -> Q  -> Q
sb mp x = case lookup x mp of
    Nothing -> error "sb"
    Just x' -> x'

mkMap :: SS -> [Q] -> (SS, [(Q, Q)])
mkMap qn0 qs = mapAccumL f qn0 qs
    where
    f qn q = (qn', (q, q'))
        where
        (q', qn') = stateSupply qn

newtype SS = SS Int
initialState :: SS
initialState = SS 0

stateSupply :: SS -> (Q, SS)
stateSupply (SS q) = (q, SS (q + 1))


runDFA :: String -> DFA -> Bool
runDFA xs dfa@(qs', s', delta', q0', f') = q' `Set.member` f'
--runDFA :: String -> DFA -> Q'
--runDFA xs dfa@(qs', s', delta', q0', f') = q'
    where
    q' = runDelta' q0' (reverse xs)
    runDelta' :: Q' -> String -> Q'
    runDelta' q [] = q
    runDelta' q (x:xs) = del (runDelta' q xs)
        where
        del y
            | Map.member y delta' && Map.member [x] d = d Map.! [x]
            | otherwise = y
            where
            d = delta' Map.! y
run :: Regex -> String -> Bool
--run :: Regex -> String -> Q'
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

test10 :: Bool
test10 = run regex1 "a"
--test1 = runDFA "" . toDFA . regexToNFA $ regex1
--
test11 :: Bool
test11 = run regex1 "abc"

regex2 :: Regex
regex2 = Cat (Chr "y") (Chr "z")

test20 :: Bool
test20 = run regex2 "yz"

test21 :: Bool
test21 = run regex2 "y"

regex3 :: Regex
regex3 = Alt regex1 regex2

test30 :: Bool
test30 = run regex3 "yz"

regex4 :: Regex
regex4 = Star regex2

regex50 :: Regex
regex50 = foldl (\acc x -> Alt x acc) Empty $ map (Chr . singleton) ['a'..'z']
regex51 = 
    Cat 
        (Cat (Star regex50) (Chr "@")) 
        (Star regex50)

