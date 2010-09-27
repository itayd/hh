{-# OPTIONS_GHC -Wall -Werror -O2 #-}

import Graphics.Vty
import Control.Exception(bracket)
import System.Exit(ExitCode(..), exitWith)
import System.IO(openFile, IOMode(ReadMode,WriteMode), hGetContents, hPutStrLn, hClose)
import System.Environment(getArgs, getEnv)
import Data.List(group, sort, sortBy, isInfixOf)
import Data.Ord(comparing)

withVty :: (Vty -> IO a) -> IO a
withVty = bracket mkVty shutdown

fromInt :: Num a => Int -> a
fromInt = fromInteger.toInteger

data Result = Chosen String | Aborted | SwitchMode

data Mode = Freq | Recent

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

parseArgs :: [String] -> IO String
parseArgs [] = fmap (++ "/.bash_history") $ getEnv "HOME"
parseArgs (a:[]) = return a
parseArgs _ = error "too many arguments"

fileLines :: String -> IO [String]
fileLines fn = fmap lines $ openFile fn ReadMode >>= hGetContents

write :: String -> IO ()
write what = do
    h <- openFile "/tmp/.hh.tmp" WriteMode
    hPutStrLn h what
    hClose h

histogram :: Ord a => [a] -> [(a,Int)]
histogram as = let mk a = (head a, length a) in map mk (group $ sort as)

freqSort :: Ord a => [a] -> [(a,Int)]
freqSort = sortBy (comparing snd) . histogram

vtyHeight :: Vty -> IO Int
vtyHeight vty = do
    bounds <- display_bounds $ terminal vty
    return $ fromInteger( toInteger $ region_height bounds ) :: IO Int

play :: Mode -> [String] -> IO ()
play mode ls = do

    let f' = case mode of
                Recent -> reverse . histogram
                Freq   -> reverse . freqSort
        f  = f' . filter (not . null)

    r <- withVty $ \vty -> do
        height <- vtyHeight vty
        select vty height (f ls) (modeTitle mode) 0 0 ""

    case r of
        Aborted -> exitWith $ ExitFailure 1
        Chosen s -> write s
        SwitchMode -> play (nextMode mode) ls

        where
            nextMode Freq = Recent
            nextMode Recent = Freq

            modeTitle Freq = "F"
            modeTitle Recent = "R"

main :: IO ()
main = getArgs >>= parseArgs >>= fileLines >>= play Freq

