{-# OPTIONS_GHC -Wall -Werror -O2 #-}

import Graphics.Vty
import Control.Exception(bracket)
import System.Exit(ExitCode(..), exitWith)
import System.IO(openFile, IOMode(ReadMode,WriteMode), hGetContents, hPutStrLn, hClose)
import System.Environment(getEnv)
import Data.List(group, sort, sortBy, isInfixOf)
import Data.Ord(comparing)
import Data.Maybe(fromJust)

data Config = Config {
    mode :: Mode,
    historyFileName :: String,
    favoritesFileName :: String
}

mkConfig :: IO Config
mkConfig = do
    h <- getEnv "HOME"
    return $ Config Freq (h ++ "/.bash_history") (h ++ "/.hh_favorites")

type FilterFunc = [String] -> [(String,Int)]

data Behaviour = Behaviour {
    mbNext :: Mode,
    mbTitle :: String,
    mbFn :: Config -> String,
    mbFunc :: FilterFunc
}

modes :: [(Mode,Behaviour)]
modes = [(Freq,      Behaviour Recent    "F" historyFileName   (reverse . freqSort) ),
         (Recent,    Behaviour Favorites "R" historyFileName   (reverse . same) ),
         (Favorites, Behaviour Freq      "*" favoritesFileName (reverse . same) )]
    where
        same = map $ \l -> (l,1)

withVty :: (Vty -> IO a) -> IO a
withVty = bracket mkVty shutdown

fromInt :: Num a => Int -> a
fromInt = fromInteger.toInteger

data Result = Chosen String | Aborted | SwitchMode

data Mode = Freq | Recent | Favorites deriving (Eq)

select :: Vty -> Int -> [(String, Int)] -> String -> Int -> Int -> String -> IO Result
select vty height ls' prefix top curr word = do
    update vty (pic_for_image . vert_cat $ status ++ items ++ fin) { pic_cursor = cursor }
    e <- next_event vty
    case e of
        EvKey KEsc []               -> quit
        EvKey (KASCII 'c') [MCtrl]  -> quit
        EvKey (KASCII 'C') [MCtrl]  -> quit
        EvKey (KASCII 'd') [MCtrl]  -> quit
        EvKey (KASCII 'D') [MCtrl]  -> quit
        EvKey (KASCII 'u') [MCtrl]  -> again top curr ""
        EvKey (KASCII 'U') [MCtrl]  -> again top curr ""
        EvKey (KASCII '\t') []      -> return SwitchMode
        EvKey KDown []              -> down
        EvKey KUp []                -> up
        EvKey KHome []              -> home
        EvKey (KASCII ch) []        -> reduce ch
        EvKey KBS []                -> enhance
        EvKey KEnter []             -> choose
        EvResize _ height'          -> select vty height' ls' prefix top curr word
        _                           -> same
        where

            again                                   = select vty height ls' prefix

            quit                                    = return Aborted

            ls                                      = filter (isInfixOf word . fst) ls'

            cursor                                  = Cursor (fromInt $ length word + length prefix + 1) 0

            status                                  = [string def_attr $ prefix ++ '>' : word]

            fin                                     = [string def_attr " "]

            items                                   = map mkLine [top..min (top + height - 2) (length ls - 1)]

            same                                    = again top curr word

            reduce ch                               = again 0 0 $ word ++ [ch]

            enhance
              | length word > 0                     = again top curr $ init word
              | otherwise                           = same

            choose
              | curr < length ls                    = return . Chosen $ fst (ls !! curr)
              | otherwise                           = same

            home                                    = again 0 0 word

            up
              | top == 0 && curr == 0               = same
              | top == curr                         = again (top - 1) (curr - 1) word
              | otherwise                           = again top (curr - 1) word

            down
              | (curr + 1) >= length ls             = same
              | (top + height - 1) == (curr + 1 )   = again (top + 1) (curr + 1) word
              | otherwise                           = again top (curr + 1) word

            mkLine n
                | n == curr                         = string sel_attr $ txt $ ls !! n
                | otherwise                         = string def_attr $ txt $ ls !! n
                where
                    txt = fst
                    sel_attr = def_attr `with_style` standout

fileLines :: String -> IO [String]
fileLines fn = catch go handler
    where
        go = fmap lines $ openFile fn ReadMode >>= hGetContents
        handler _ = return []

write :: String -> IO ()
write what = do
    h <- openFile "/tmp/.hh.tmp" WriteMode
    hPutStrLn h what
    hClose h

histogram :: FilterFunc
histogram as = let mk a = (head a, length a) in map mk (group $ sort as)

freqSort :: FilterFunc
freqSort = sortBy (comparing snd) . histogram

vtyHeight :: Vty -> IO Int
vtyHeight vty = do
    bounds <- display_bounds $ terminal vty
    return $ fromInteger( toInteger $ region_height bounds ) :: IO Int

play :: Config -> IO ()
play cfg = do
    ls <- fileLines fn
    r <- withVty $ \vty -> do
        height <- vtyHeight vty
        select vty height (func ls) title 0 0 ""
    case r of
        Aborted -> exitWith $ ExitFailure 1
        Chosen s -> write s
        SwitchMode -> play $ cfg { mode = next }

        where
            b     = fromJust $ lookup (mode cfg) modes
            next  = mbNext b
            title = mbTitle b
            fn    = mbFn b cfg
            func  = mbFunc b . filter (not . null)

main :: IO ()
main = mkConfig >>= play

