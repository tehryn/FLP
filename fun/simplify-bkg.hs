import System.Environment
import System.Exit
import System.IO
import Control.Monad
import Data.List

-- Stores program settings
data Settings = Settings {
    argument :: Integer,
    inputFile :: String,
    settingsErrorCode :: Int
} deriving ( Show )

-- G = (N, T, P, S)
data Grammer = Grammer {
    nonTerminals :: String,
    terminals :: String,
    rules :: [ Rule ],
    starting :: Char,
    valide :: Bool
}

data Rule = Rule {
    leftSide :: Char,
    rightSide :: String
}

-- Stores program settings
data Input = Input {
    grammer :: Grammer,
    inputErrorCode :: Int
} deriving ( Show )

_strRules :: [ String ] -> [ Rule ] -> [ String ]
_strRules l []    = l
_strRules l (e:r) = _strRules ( l ++ [ show e ] ) r

_strTerms :: String -> String -> String
_strTerms l "" = drop 1 l
_strTerms l (c:s) = _strTerms ( l ++ [',',c] ) s


instance Show Grammer
    where
        show Grammer {
            nonTerminals=nonTerminals,
            terminals=terminals,
            rules=rules,
            starting=starting,
            valide=valide
        } = _strTerms "" nonTerminals ++ "\n" ++ _strTerms "" terminals ++ "\n" ++ intercalate "\n" ( _strRules [] rules ) ++ "\n"

instance Show Rule
    where
        show Rule { leftSide = leftSide, rightSide = rightSide } = leftSide : "->" ++ rightSide

invalidArguments :: Int
invalidArguments = 1

unknownError :: Int
unknownError = 2

invalidInput :: Int
invalidInput = 3

failed :: Int -> IO ()
failed n = do
    case n of
        1 -> print "Invalid use of application. Please run program as simplify-bkg -i|-1|-2 [ input_file ]"
        2 -> print "Unspecified error"
        3 -> print "Invalid input, please check syntax of input grammer."
        _ -> print "Unexpected error"
    exitWith ( ExitFailure n )

main :: IO ()
main = do
    args <- getArgs
    let settings = parseArguments args
    let err_set = checkSettigs settings
    when ( err_set /= 0 ) $ failed err_set
    let action = getArgument settings
    source <- loadInput settings
    let g = loadGrammer source
    let err_g = checkGrammer g
    when ( err_g /= 0 ) $ failed err_g
    case action of
        0 -> print g
        1 -> print ( simplify g False )
        2 -> print ( simplify g True )
        _ -> failed unknownError
    return ()


checkSettigs :: Settings -> Int
checkSettigs (Settings _ _ err) = err

checkGrammer :: Grammer -> Int
checkGrammer (Grammer _ _ _ _ ok) = if ok then 0 else invalidInput

getArgument :: Settings -> Integer
getArgument (Settings arg _ _) = arg

-- parsing program arguments
parseArguments :: [ String ] -> Settings
parseArguments [] = Settings (-1) "" invalidArguments
parseArguments [ i ]
    | i == "-i" = Settings 0 "" 0
    | i == "-1" = Settings 1 "" 0
    | i == "-2" = Settings 2 "" 0
    | otherwise = Settings (-1) "" invalidArguments
parseArguments [ i, n ]
    | i == "-i" = Settings 0 n 0
    | i == "-1" = Settings 1 n 0
    | i == "-2" = Settings 2 n 0
    | otherwise = Settings (-1) n invalidArguments
parseArguments _ = Settings (-1) "" invalidArguments

loadInput :: Settings -> IO String
loadInput ( Settings _ fileName _ )
    | fileName == "" = hGetContents stdin
    | otherwise = loadFile fileName
    where
        loadFile input = do
            hFile <- openFile input ReadMode
            hGetContents hFile


loadGrammer :: String -> Grammer
loadGrammer source = let
        validLines = length ( lines source ) >= 4
        ( n:t:s:r ) = if validLines then lines source else [ "", "", "", "" ]
        validTerms = ( n /= "" ) && ( t /= "" ) && ( s /= "" ) && checkTerms t n s
        n' = if validTerms then [ c|c <- n, c /= ',' ] else ""
        t' = if validTerms then [ c|c <- t, c /= ',' ] else ""
        validGrammer = validTerms && validLines && isUnique n' && isUnique t' && checkRules n' t' r s'
        r' = if validGrammer then loadRules r else []
        s' = if validTerms && validLines then head s else 'S'
        g = Grammer n' t' r' s' validGrammer
    in g

loadRules :: [ String ] -> [ Rule ]
loadRules source = readRules source []
    where
        readRules [] r = r
        readRules (str:l) r = readRules l ( readRule str : r )
        readRule str = Rule ( head ( left str ) ) (right str)
        left  str = parseLeft str ""
        parseLeft "" _ = error "Error: invalid syntax of rules."
        parseLeft (c:s) res = if c == '-' then res else parseLeft s ( res ++  [c] )
        right str = parseRight str "" False
        parseRight "" s b = if b then s else error "Error: invalid syntax of rules."
        parseRight (c:s) res b
            | b            = parseRight s ( res ++  [c] ) True
            | c == '>'     = parseRight s "" True
            | otherwise    = parseRight s "" False

checkRules :: String -> String -> [ String ] -> Char -> Bool
checkRules n t r s = checkSyntaxAll r n t && checkStartingAll r s
    where
        checkSyntaxAll [] _ _ = True
        checkSyntaxAll (rule:rest) nonTerms terms = checkSyntaxRule rule nonTerms terms ( 0 :: Integer ) && checkSyntaxAll rest nonTerms terms
        -- 0 zacinam cist levou stranu
        -- 1 ctu levou stranu a uz mam jeden neterminal, musi prijit '-'
        -- 2 ocekavam '>'
        -- 3 zacinam cist pravou stranu
        -- 4 ctu levou stranu
        checkSyntaxRule "" _ _ state = state == 4
        checkSyntaxRule (c:str) nonTerms terms state
            | state == 0 = elem c nonTerms && checkSyntaxRule str nonTerms terms 1
            | state == 1 = c == '-' && checkSyntaxRule str nonTerms terms 2
            | state == 2 = c == '>' && checkSyntaxRule str nonTerms terms 3
            | otherwise = elem c (terms ++ nonTerms) && checkSyntaxRule str nonTerms terms 4
        checkStartingAll [] _ = False
        checkStartingAll (rule:rest) startNonTerminal = checkStartingRule rule startNonTerminal || checkStartingAll rest startNonTerminal
        checkStartingRule "" _ = False
        checkStartingRule (c:str) startNonTerminal
            | c == s   = True
            | c == '-'  = False
            | otherwise = checkStartingRule str startNonTerminal

checkTerms :: String -> String -> String -> Bool
checkTerms t n s = checkTerminals t False && checkNonTerminals n False && checkStarting s n
    where
        checkTerminals "" cond = cond
        checkTerminals (c:l) False = elem c ['a' .. 'z'] && checkTerminals l True
        checkTerminals (c:l) True = c == ',' && checkTerminals l False
        checkNonTerminals "" cond = cond
        checkNonTerminals (c:l) False = elem c ['A' .. 'Z'] && checkNonTerminals l True
        checkNonTerminals (c:l) True = c == ',' && checkNonTerminals l False
        checkStarting symbol nonTerm = ( length symbol == 1 ) && elem ( head symbol ) nonTerm && head symbol /= ','

isUnique :: Eq a => [ a ] -> Bool
isUnique [] = True
isUnique (e:l) = notElem e l && isUnique l

unique :: Eq a => [ a ] -> [ a ]
unique l  = procUnique l []
    where
        procUnique [] res = res
        procUnique (e:i) res = if elem e res then procUnique i res else procUnique i ( e : res )

getRuleSideRight :: Rule -> String
getRuleSideRight (Rule _ r) = r

getRuleSideLeft  :: Rule -> Char
getRuleSideLeft (Rule l _) = l

simplify :: Grammer -> Bool -> Grammer
simplify (Grammer nonTerms terms rules start _ ) cont = let
        -- parametry: gramatika, Ni-1, Ni
        n = algNotEmpty "" "" False
        r = algNotEmptyRules n rules []
    in Grammer n terms r start True
    where
        algNotEmpty _ _ False = algNotEmpty "" (getNextNotEmpty rules "" "") True
        algNotEmpty prev curr True = if length prev == length curr then prev else algNotEmpty curr (getNextNotEmpty rules "" curr) True
        getNextNotEmpty [] curr _ = curr
        getNextNotEmpty (r:rs) curr ni = if notElem ( getRuleSideLeft r ) curr && getNextNotEmptyAppend ( getRuleSideRight r ) ni
            then getNextNotEmpty rs ( getRuleSideLeft r : curr ) ni
            else getNextNotEmpty rs curr ni
        getNextNotEmptyAppend "" _ = True
        getNextNotEmptyAppend (c:str) ni = ( elem c ni || elem c terms ) && getNextNotEmptyAppend str ni
        algNotEmptyRules _ [] res = res
        algNotEmptyRules ni (r:rs) res = if elem ( getRuleSideLeft r ) ni && getNextNotEmptyAppend ( getRuleSideRight r ) ni
            then algNotEmptyRules ni rs ( r : res )
            else algNotEmptyRules ni rs res
