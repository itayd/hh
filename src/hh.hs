{-# OPTIONS_GHC -Wall -Werror -O2 #-}

import Graphics.Vty
import Control.Exception(bracket)
import System.Exit(ExitCode(..), exitWith)
import System.IO(withFile, IOMode(ReadMode,WriteMode,AppendMode), hGetLine, hPutStrLn, hPutStr)
import System.Environment(getEnv, getArgs)
import System.Directory(doesFileExist)
import Data.List(group, nubBy, sort, sortBy, isPrefixOf, delete)
import Data.Ord(comparing)
import Data.Maybe(fromJust)
import Data.Function(on)
import Text.Printf(printf)

import qualified System.IO.Strict as Strict

atHome :: FilePath -> (FilePath -> FilePath)
atHome = printf . ("%s/" ++)

defOutputFile, defHistFileName, defFavFileName, rcFileName :: FilePath -> FilePath
rcFileName = atHome ".hhrc"
defFavFileName = atHome ".hh_favorites"
defHistFileName = atHome ".bash_history"
defOutputFile = atHome ".hh.last"

data Config = Config {
    currMode            :: Mode,
    historyFileName     :: FilePath,
    favoritesFileName   :: FilePath,
    outputFileName      :: FilePath
} deriving (Show,Read)

getHome :: IO FilePath
getHome = getEnv "HOME"

loadConfig :: IO Config
loadConfig = do
    home <- getHome
    let rcFile = rcFileName home in catch (load rcFile) $ def home
        where
            load fn = fmap read (withFile fn ReadMode hGetLine) :: IO Config
            def h _ = return $ Config Freq (defHistFileName h) (defFavFileName h) (defOutputFile h)

saveConfig :: Config -> IO ()
saveConfig cfg = do
    home <- getHome
    withFile (rcFileName home) WriteMode $ flip hPutStrLn (show cfg)

type Item = (String, Int)

type FilterFunc = [String] -> [Item]
type ShowFunc = Bool -> Item -> Image

data Mode = Freq | Recent | Favorites deriving (Eq, Show, Read, Bounded, Enum)

nextMode :: Mode -> Mode
nextMode m
    | m == maxBound = m
    | otherwise     = succ m

prevMode :: Mode -> Mode
prevMode m
    | m == minBound = m
    | otherwise     = pred m

data Behaviour = Behaviour {
    mbTitle     :: String,
    mbFn        :: Config -> String,
    mbFunc      :: FilterFunc,
    mbShow      :: ShowFunc
}

modes :: [(Mode,Behaviour)]
modes = [(Freq,      Behaviour "freq"      historyFileName   (reverse . freqSort) onlyStr  ),
         (Recent,    Behaviour "recent"    historyFileName   (reverse . same)     withN    ),
         (Favorites, Behaviour "favs"      favoritesFileName (reverse . same)     onlyStr  )]
    where
        same                  = flip zip [1..]

        sel_attr              = def_attr `with_style` standout

        onlyStr  False (s, _) = string def_attr s
        onlyStr  True  (s, _) = string sel_attr s

        withNstr       (s, n) = '[' : show n ++ "] " ++ s
        withN    False i      = string def_attr $ withNstr i
        withN    True  i      = string sel_attr $ withNstr i


withVty :: (Vty -> IO a) -> IO a
withVty = bracket mkVty shutdown

fromInt :: Num a => Int -> a
fromInt = fromInteger.toInteger

data Result = Chosen String | Aborted | PrevMode String | NextMode String | Refresh SelectState

maxPrefix :: Eq a => [[a]] -> [a] -> [a]
maxPrefix [] p = p
maxPrefix ps p
    | length p > minimum (map length ps) = p
    | all (isPrefixOf next) ps = maxPrefix ps next
    | otherwise = p
    where
        next = p ++ [head ps !! length p]

type SelectState = (Int, Int, String)

select :: Vty -> Config -> Int -> [Item] -> ShowFunc -> String -> SelectState -> IO Result
select vty cfg height ls' showFunc prefix state@(top, curr, word) = do
    update vty (pic_for_image . vert_cat $ status ++ items ++ fin) { pic_cursor = cursor }
    e <- next_event vty
    case e of
        EvKey KEsc []               -> quit
        EvKey (KASCII 'c')  [MCtrl] -> quit
        EvKey (KASCII 'C')  [MCtrl] -> quit
        EvKey (KASCII 'd')  [MCtrl] -> quit
        EvKey (KASCII 'D')  [MCtrl] -> quit
        EvKey (KASCII 'u')  [MCtrl] -> again (top, curr, "")
        EvKey (KASCII 'U')  [MCtrl] -> again (top, curr, "")
        EvKey (KASCII '\t') []      -> complete
        EvKey (KASCII 'a')  [MCtrl] -> favAdd
        EvKey (KASCII 'r')  [MCtrl] -> favRem
        EvKey KRight        []      -> return $ NextMode word
        EvKey KLeft         []      -> return $ PrevMode word
        EvKey KDown         []      -> down
        EvKey KUp           []      -> up
        EvKey KHome         []      -> home
        EvKey KEnd          []      -> end
        EvKey KPageUp       []      -> pgup
        EvKey KPageDown     []      -> pgdn
        EvKey (KASCII ch)   []      -> reduce ch
        EvKey KBS           []      -> enhance
        EvKey KEnter        []      -> choose
        EvResize _ _                -> reload
        _                           -> same
    where

        again                                   = select vty cfg height ls' showFunc prefix

        same                                    = again state

        reload                                  = return $ Refresh state

        quit                                    = return Aborted

        ls                                      = nubBy ((==) `on` fst) $ filter (isPrefixOf word . fst) ls'

        cursor                                  = Cursor (fromInt $ length word + length prefix + 1) 0

        status                                  = [string def_attr (prefix ++ ">") <|> string (def_attr `with_style` bold) word]

        fin                                     = [string def_attr " "]

        items                                   = map mkLine [top..min (top + height - 2) (length ls - 1)]

        complete                                = let word' = maxPrefix (map fst ls) word
                                                  in  again (0, 0, word')

        reduce ch                               = again (0, 0, word ++ [ch])

        enhance
          | length word > 0                     = again (0, 0, init word)
          | otherwise                           = same

        choose
          | curr < length ls                    = return . Chosen $ fst (ls !! curr)
          | otherwise                           = same

        favAdd
          | null ls                             = same
          | otherwise                           = do
                _ <- favRem
                withFile favFn AppendMode $ flip hPutStrLn (fst $ ls !! curr)
                reload

        favRem
          | null ls                             = same
          | otherwise                           = do
                ll <- readFileLines favFn
                withFile favFn WriteMode $ \h ->
                   let ll' = delete (fst $ ls !! curr) ll
                   in  hPutStr h $ unlines ll'
                return $ Refresh (top, 0, word)

        favFn                                   = favoritesFileName cfg

        home                                    = again (0, 0, word)

        end
          | null ls                             = same
          | otherwise                           = let y' = max (length ls - height + 1) 0
                                                  in  again (y', length ls - 1, word)

        up
          | top == 0 && curr == 0               = same
          | top == curr                         = again (top - 1, curr - 1, word)
          | otherwise                           = again (top,     curr - 1, word)

        down
          | (curr + 1) >= length ls             = same
          | (top + height - 1) == (curr + 1 )   = again (top + 1, curr + 1, word)
          | otherwise                           = again (top,     curr + 1, word)

        pgup                                    = let y' = max (top - height) 0
                                                  in  again (y',  y', word)

        pgdn
          | top + height >= length ls           = end
          | otherwise                           = let y' = min (top + height) (length ls - height)
                                                  in  again (y', y', word)

        mkLine n                                = showFunc (n == curr) $ ls !! n

readFileLines :: FilePath -> IO [String]
readFileLines fn = do
    e <- doesFileExist fn
    if e
      then fmap lines $ Strict.readFile fn
      else return []

histogram :: FilterFunc
histogram as = let mk a = (head a, length a)
               in  map mk (group $ sort as)

freqSort :: FilterFunc
freqSort = sortBy (comparing snd) . histogram

vtyHeight :: Vty -> IO Int
vtyHeight vty = do
    bounds <- display_bounds $ terminal vty
    return $ fromInteger( toInteger $ region_height bounds ) :: IO Int

play :: SelectState -> Config -> IO ()
play state cfg = do
    ls <- readFileLines fn
    r <- withVty $ \vty -> do
        height <- vtyHeight vty
        select vty cfg height (func ls) showFunc title state
    case r of
        Aborted                     -> exitWith $ ExitFailure 1
        Chosen   cmd                -> writeFile (outputFileName cfg) (cmd ++ "\n") >> saveConfig cfg
        NextMode word'              -> play (0, 0, word') $ cfg { currMode = next }
        PrevMode word'              -> play (0, 0, word') $ cfg { currMode = prev }
        Refresh  state'             -> play state' cfg
    where
        b           = fromJust $ lookup (currMode cfg) modes
        next        = nextMode $ currMode cfg
        prev        = prevMode $ currMode cfg
        title       = mbTitle b
        fn          = mbFn b cfg
        showFunc    = mbShow b
        func        = mbFunc b . filter (not . null)

main :: IO ()
main = do
    word <- fmap unwords getArgs
    cfg <- loadConfig
    play (0, 0, word) cfg

