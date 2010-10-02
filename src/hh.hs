{-# OPTIONS_GHC -Wall -Werror -O2 #-}

import Graphics.Vty
import Control.Exception(bracket)
import System.Exit(ExitCode(..), exitWith)
import System.IO(withFile, IOMode(ReadMode,WriteMode,AppendMode), hGetLine, hPutStrLn, hPutStr)
import System.Environment(getEnv, getArgs)
import System.Directory(doesFileExist)
import Data.List(group, nubBy, sort, sortBy, isPrefixOf, delete, intersect, find)
import Data.Ord(comparing)
import Data.Maybe(fromJust)
import Data.Function(on)
import Text.Printf(printf)
import System.Console.GetOpt
import Control.Monad(forM_, unless)

import qualified System.IO.Strict as Strict

version :: String
version = "0.2"

atHome :: FilePath -> (FilePath -> FilePath)
atHome = printf . ("%s/" ++)

defOutputFile, defHistFileName, defFavFileName, rcFileName :: FilePath -> FilePath
rcFileName      = atHome ".hhrc"
defFavFileName  = atHome ".hh_favorites"
defHistFileName = atHome ".bash_history"
defOutputFile   = atHome ".hh.last"

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
type ShowFunc = Bool -> Bool -> Item -> Image
type IsFavFunc = String -> Bool

data Mode = Favorites | Freq | Recent deriving (Eq, Show, Read, Bounded, Enum)

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
         (Recent,    Behaviour "recent"    historyFileName   (reverse . same)     onlyStr  ),
         (Favorites, Behaviour "favs"      favoritesFileName (reverse . same)     onlyStr  )]
    where
        same                  = flip zip [1..]

        sel_attr              = def_attr `with_style` standout
        fav_attr              = def_attr `with_style` bold
        fav_sel_attr          = sel_attr `with_style` bold

        onlyStr  False False = string def_attr      . fst
        onlyStr  True  False = string fav_attr      . fst
        onlyStr  False True  = string sel_attr      . fst
        onlyStr  True  True  = string fav_sel_attr  . fst

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

select :: Vty -> Config -> Int -> [Item] -> IsFavFunc -> ShowFunc -> String -> SelectState -> IO Result
select    vty    cfg       height ls'       isFav        showFunc    prefix    state@(top, curr, word) = do
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
        EvKey KUp           [MCtrl] -> prevFav
        EvKey KDown         [MCtrl] -> nextFav
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

        again                                   = select vty cfg height ls' isFav showFunc prefix

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
                return $ Refresh (top, min curr (max 0 $ length ls - 2), word)

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

        pgup                                    = let y' = max (top - height) 0 -- can't use jumpTo, always stick to the top
                                                  in  again (y',  y', word)

        pgdn                                    = jumpTo $ top + height

        indexedFavs                             = filter (isFav . snd) (zip [0..] $ map fst ls)
        gotoFav Nothing                         = same
        gotoFav (Just (curr',_))                = jumpTo curr'
        nextFav                                 = gotoFav $ find ((curr <) . fst) indexedFavs
        prevFav                                 = gotoFav $ find ((curr >) . fst) (reverse indexedFavs)

        mkLine n                                = let which = ls !! n
                                                  in  showFunc (isFav $ fst which) (n == curr) which

        jumpTo curr'
            | curr'' < top                      = next $ max 0 (curr'' - height + 2)
            | curr'' > top + height - 2         = next $ min (length ls - height + 1) curr''
            | otherwise                         = next top
            where
                curr''                          = min (max curr' 0) (length ls - 1)
                next top'                       = again (top', curr'', word)

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

play :: SelectState -> Config -> IO Config
play state cfg = do
    favs <- readFileLines (favoritesFileName cfg)
    ls <- readFileLines fn
    r <- withVty $ \vty -> do
        height <- vtyHeight vty
        select vty cfg height (func ls) (flip elem favs) showFunc title state
    case r of
        Aborted                     -> exitWith $ ExitFailure 1
        Chosen   cmd                -> writeFile (outputFileName cfg) (cmd ++ "\n") >> return cfg
        NextMode word'              -> play (0, 0, word') $ cfg { currMode = next }
        PrevMode word'              -> play (0, 0, word') $ cfg { currMode = prev }
        Refresh  state'             -> play state' cfg
    where
        b                            = fromJust $ lookup (currMode cfg) modes
        next                         = nextMode $ currMode cfg
        prev                         = prevMode $ currMode cfg
        title                        = mbTitle b
        fn                           = mbFn b cfg
        showFunc                     = mbShow b
        func                         = mbFunc b . filter (not . null)

data OptFlag = Version | Help | DontSaveConfig | Init deriving (Eq)

options :: [OptDescr OptFlag]
options = [ Option "v"     ["version"]      (NoArg Version)         "show version number",
            Option "h?"    ["help"]         (NoArg Help)            "show help",
            Option "n"     ["no-save-cfg"]  (NoArg DontSaveConfig)  "do not save configuration on exit",
            Option "i"     ["init"]         (NoArg Init)            "create default rc file if non exists" ]

header :: String
header  = "Usage: hh [opts...] words"

parseArgs :: [String] -> IO (String, [OptFlag])
parseArgs args = case getOpt Permute options args of
        (flags ,n, [])      -> return ( unwords n, flags )
        (_,     _, errs)    -> ioError $ userError (concat errs ++ usageInfo header options)

handleOpt :: OptFlag -> IO ()
handleOpt Version   = putStrLn version
handleOpt Help      = putStrLn $ usageInfo header options
handleOpt Init      = loadConfig >>= saveConfig
handleOpt _         = undefined

main :: IO ()
main = do
    ( word, opts ) <- getArgs >>= parseArgs
    let showStoppers = intersect [Version, Help, Init] opts
    if not . null $ showStoppers
        then forM_ opts handleOpt
        else loadConfig >>= play (0, 0, word) >>= unless (DontSaveConfig `elem` opts) . saveConfig

