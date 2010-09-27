{-# OPTIONS_GHC -Wall -Werror -O2 #-}

import Graphics.Vty
import Control.Exception(bracket)
import System.Exit(ExitCode(..), exitWith)
import System.IO(openFile, IOMode(ReadMode,WriteMode), hGetContents, hPutStrLn, hClose)
import System.Environment(getArgs, getEnv)
import Data.List(group, sort, sortBy, isPrefixOf)
import Data.Ord(comparing)

withVty :: (Vty -> IO a) -> IO a
withVty = bracket mkVty shutdown

select :: Vty -> Int -> [(String, Int)] -> Int -> Int -> String -> IO (Maybe String)
select vty height ls' top curr word = do
    update vty $ pic_for_image . vert_cat $ items ++ padding ++ status
    e <- next_event vty
    case e of
        EvKey KEsc []               -> quit
        EvKey (KASCII 'c') [MCtrl]  -> quit
        EvKey (KASCII 'C') [MCtrl]  -> quit
        EvKey KDown []              -> down
        EvKey KUp []                -> up
        EvKey KHome []              -> home
        EvKey (KASCII ch) []        -> reduce ch
        EvKey KBS []                -> enhance
        EvKey KEnter []             -> choose
        _                           -> same
        where

            padding = let vi = min (top + height - 2) (length ls - 1)
                          np = height - vi - 2
                      in replicate np $ string def_attr " "

            again = select vty height ls'

            quit                                    = return Nothing

            ls                                      = filter (isPrefixOf word . fst) ls'

            status                                  = [string def_attr $ '>' : word]

            items                                   = map mkLine [top..min (top + height - 2) (length ls - 1)]

            same                                    = again top curr word

            reduce ch                               = again 0 0 $ word ++ [ch]

            enhance
              | length word > 0                     = again top curr $ init word
              | otherwise                           = same

            choose                                  = return . Just $ fst (ls !! curr)

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

main :: IO ()
main = do
    ls <- fmap (filter $ not . null) ( getArgs >>= parseArgs >>= fileLines)

    r <- withVty $ \vty -> do
        bounds <- display_bounds $ terminal vty
        let height = fromInteger (toInteger $ region_height bounds) :: Int
        select vty height (reverse $ freqSort ls) 0 0 ""

    case r of
        Nothing -> exitWith $ ExitFailure 1
        Just s -> write s

