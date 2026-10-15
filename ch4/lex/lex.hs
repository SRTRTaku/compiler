import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List (singleton)

type S = String -- Sigma
type Q = Int -- State

-- Q -> [S -> [Q]]
--type Delta = [(Q,[(S,[Q])])] 
type Delta = Map.Map Q (Map.Map S [Q])

type NFA = ([Q], [S], Delta, Q, [Q])
--type NFA = NFA {
--    qs :: [Q],
--    s :: [S],
--    delta :: Delta,
--    q0 :: Q,
--    f :: [Q]
--}

type Q' = [Q]
-- Q' -> [S -> Q']
--type Delta' = [(Q', [(S, Q')])]
type Delta' = Map.Map Q' (Map.Map S Q')

type DFA = ([Q'], [S], Delta', Q', [Q'])
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
    qs = [0, 1]
    delta = Map.empty
    q0 = 0
    f = [1]
regexToNFA (Chr x) = (qs, sigma, delta, q0, f)
    where
    qs = [0, 1]
    delta = Map.fromList [(0, Map.fromList [(x, [1])])]
    q0 = 0
    f = [1]
regexToNFA r = error "not yet implementes"


runDFA :: String -> DFA -> Bool
runDFA xs dfa@(qs', s', delta', q0', f') = q' `elem` f'
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
    q0' = cl delta [q0]
    (qs', delta') = subsets ([q0'], [], Map.empty)
    f' = [[1,2,3]]

cl :: Delta -> [Q] -> [Q]
cl delta ps = Set.toList $ cl' (Set.fromList ps)
    where
    cl' :: Set.Set Q -> Set.Set Q
    cl' xs
        | xs' == xs = xs
        | otherwise = cl' xs'
        where
        --step xs = Set.fromList $ concat [eTrans delta x |x <- Set.toList xs]
        --xs' = Set.union xs (step xs)
        xs' = Set.foldr (\x acc -> Set.union acc (eTrans delta x)) xs xs

eTrans :: Delta -> Q -> Set.Set Q
eTrans delta p
    | Map.member p delta && Map.member "" d
        = Set.fromList $ d Map.! ""
    | otherwise = Set.empty
    where
    d = delta Map.! p

subsets :: ([Q'], [Q'], Delta') -> ([Q'], Delta')
subsets = undefined

--
--
regex1 :: Regex
regex1 = Chr "a" -- a

test1 :: Bool
test1 = run regex1 "xyz"
--test1 = runDFA "" . toDFA . regexToNFA $ regex1
