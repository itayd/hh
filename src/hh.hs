{-# OPTIONS_GHC -Wall -Werror -O2 #-}

import Graphics.Vty
import Control.Exception(bracket)
import System.Exit(ExitCode(..), exitWith)
import System.IO(withFile, openFile, IOMode(ReadMode,WriteMode), hGetContents, hGetLine, hPutStrLn )
import System.Environment(getEnv, getArgs)
import Data.List(group, nubBy, sort, sortBy, isPrefixOf)
import Data.Ord(comparing)
import Data.Maybe(fromJust)
import Data.Function(on)

data Config = Config {
    mode                :: Mode,
    historyFileName     :: String,
    favoritesFileName   :: String
} deriving (Show,Read)

getHome :: IO String
getHome = getEnv "HOME"

loadConfig :: IO Config
loadConfig = do
    home <- getHome
    let rcFile = home ++ "/.hhrc"
    catch (load rcFile) $ def home
        where
            load fn = fmap read (withFile fn ReadMode hGetLine) :: IO Config
            def h _ = return $ Config Freq (h ++ "/.bash_history") (h ++ "/.hh_favorites")

saveConfig :: Config -> IO ()
saveConfig cfg = do
    home <- getHome
    withFile (home ++ "/.hhrc" ) WriteMode $ flip hPutStrLn (show cfg)

type Item = (String, Int)

type FilterFunc = [String] -> [Item]
type ShowFunc = Bool -> Item -> Image

data Mode = Freq | Recent | Favorites deriving (Eq,Show,Read)

data Behaviour = Behaviour {
    mbPrev      :: Mode,
    mbNext      :: Mode,
    mbTitle     :: String,
    mbFn        :: Config -> String,
    mbFunc      :: FilterFunc,
    mbShow      :: ShowFunc
}

modes :: [(Mode,Behaviour)]
modes = [(Freq,      Behaviour Favorites  Recent    "F" historyFileName   (reverse . freqSort) onlyStr  ),
         (Recent,    Behaviour Freq       Favorites "R" historyFileName   (reverse . same)     withN    ),
         (Favorites, Behaviour Recent     Freq      "*" favoritesFileName (reverse . same)     onlyStr  )]
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

data Result = Chosen String | Aborted | PrevMode String | NextMode String

maxPrefix :: Eq a => [[a]] -> [a] -> [a]
maxPrefix [] p = p
maxPrefix ps p
    | length p > minimum (map length ps) = p
    | all (isPrefixOf next) ps = maxPrefix ps next
    | otherwise = p
    where
        next = p ++ [head ps !! length p]

select :: Vty -> Int -> [Item] -> ShowFunc -> String -> Int -> Int -> String -> IO Result
select vty height ls' showFunc prefix top curr word = do
    update vty (pic_for_image . vert_cat $ status ++ items ++ fin) { pic_cursor = cursor }
    e <- next_event vty
    case e of
        EvKey KEsc []               -> quit
        EvKey (KASCII 'c')  [MCtrl] -> quit
        EvKey (KASCII 'C')  [MCtrl] -> quit
        EvKey (KASCII 'd')  [MCtrl] -> quit
        EvKey (KASCII 'D')  [MCtrl] -> quit
        EvKey (KASCII 'u')  [MCtrl] -> again top curr ""
        EvKey (KASCII 'U')  [MCtrl] -> again top curr ""
        EvKey (KASCII '\t') []      -> complete
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
        EvResize _ height'          -> select vty height' ls' showFunc prefix top curr word
        _                           -> same
    where

        again                                   = select vty height ls' showFunc prefix

        same                                    = again top curr word

        quit                                    = return Aborted

        ls                                      = nubBy ((==) `on` fst) $ filter (isPrefixOf word . fst) ls'

        cursor                                  = Cursor (fromInt $ length word + length prefix + 1) 0

        status                                  = [string def_attr $ prefix ++ '>' : word ]

        fin                                     = [string def_attr " "]

        items                                   = map mkLine [top..min (top + height - 2) (length ls - 1)]

        complete                                = do
            let mp = maxPrefix (map fst ls) word
            again 0 0 mp

        reduce ch                               = again 0 0 $ word ++ [ch]

        enhance
          | length word > 0                     = again top curr $ init word
          | otherwise                           = same

        choose
          | curr < length ls                    = return . Chosen $ fst (ls !! curr)
          | otherwise                           = same

        home                                    = again 0 0 word

        end
          | null ls                             = same
          | otherwise                           = let y' = max (length ls - height + 1) 0 in again y' (length ls - 1) word

        up
          | top == 0 && curr == 0               = same
          | top == curr                         = again (top - 1) (curr - 1) word
          | otherwise                           = again top (curr - 1) word

        down
          | (curr + 1) >= length ls             = same
          | (top + height - 1) == (curr + 1 )   = again (top + 1) (curr + 1) word
          | otherwise                           = again top (curr + 1) word

        pgup                                    = let y' = max (top - height) 0 in again y' y' word

        pgdn
          | top + height >= length ls           = end
          | otherwise                           = let y' = min (top + height) (length ls - height) in again y' y' word

        mkLine n                                = showFunc (n == curr) $ ls !! n

fileLines :: String -> IO [String]
fileLines fn = catch go handler
    where
        go = fmap lines $ openFile fn ReadMode >>= hGetContents
        handler _ = return []

write :: String -> IO ()
write = withFile "/tmp/.hh.tmp" WriteMode . flip hPutStrLn

histogram :: FilterFunc
histogram as = let mk a = (head a, length a) in map mk (group $ sort as)

freqSort :: FilterFunc
freqSort = sortBy (comparing snd) . histogram

vtyHeight :: Vty -> IO Int
vtyHeight vty = do
    bounds <- display_bounds $ terminal vty
    return $ fromInteger( toInteger $ region_height bounds ) :: IO Int

play :: String -> Config -> IO ()
play word cfg = do
    ls <- fileLines fn
    r <- withVty $ \vty -> do
        height <- vtyHeight vty
        select vty height (func ls) showFunc title 0 0 word
    case r of
        Aborted -> exitWith $ ExitFailure 1
        Chosen s -> write s >> saveConfig cfg
        NextMode s -> play s $ cfg { mode = next }
        PrevMode s -> play s $ cfg { mode = prev }
    where
        b           = fromJust $ lookup (mode cfg) modes
        next        = mbNext b
        prev        = mbPrev b
        title       = mbTitle b
        fn          = mbFn b cfg
        showFunc    = mbShow b
        func        = mbFunc b . filter (not . null)

main :: IO ()
main = do
    word <- fmap unwords getArgs
    cfg <- loadConfig
    play word cfg

