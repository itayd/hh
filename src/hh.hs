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

select :: Vty -> Int -> Int -> Int -> String -> [(String, Int)] -> IO (Maybe String)
select vty height top curr word ls' = do
    update vty $ pic_for_image . vert_cat $ map mkLine [top..min (top + height) (length ls - 1)]
    e <- next_event vty
    case e of
        EvKey KEsc []         -> return Nothing
        EvKey KDown []        -> down
        EvKey KUp []          -> up
        EvKey KHome []        -> select vty height 0 0 word ls'
        EvKey (KASCII ch) []  -> reduce ch
        EvKey KBS []          -> enhance
        EvKey KEnter []       -> choose
        _                     -> select vty height top curr word ls'
        where
            ls = filter (isPrefixOf word . fst) ls'

            same      = select vty height top curr word ls'

            reduce ch = select vty height 0 0 (word ++ [ch]) ls'

            enhance
              | length word > 0                 = select vty height top curr (init word) ls'
              | otherwise                       = same

            choose = return . Just $ fst (ls !! curr)

            up
              | top == 0 && curr == 0           = same
              | top == curr                     = select vty height (top - 1) (curr - 1) word ls'
              | otherwise                       = select vty height top (curr - 1) word ls'

            down
              | (curr + 1) >= length ls         = same
              | (top + height) == (curr + 1 )   = select vty height (top + 1) (curr + 1) word ls'
              | otherwise                       = select vty height top (curr + 1) word ls'

            mkLine n
                | n == curr = string sel_attr $ txt $ ls !! n
                | otherwise = string def_attr $ txt $ ls !! n
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
        select vty height 0 0 "" $ reverse $ freqSort ls

    case r of
        Nothing -> exitWith $ ExitFailure 1
        Just s -> write s

