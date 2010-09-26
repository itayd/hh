{-# OPTIONS_GHC -Wall -Werror -O2 #-}

import Graphics.Vty
import Control.Exception(bracket)
import System.Exit(ExitCode(..), exitWith)
import System.IO(openFile, IOMode(ReadMode,WriteMode), hGetContents, hPutStrLn, hClose)
import System.Environment(getArgs, getEnv)
import Data.List(group, sort, sortBy)
import Data.Ord(comparing)

withVty :: (Vty -> IO a) -> IO a
withVty = bracket mkVty shutdown

select :: Vty -> Int -> Int -> Int -> [(String, Int)] -> IO (Maybe String)
select vty height top curr ls = do
    update vty $ pic_for_image . vert_cat $ map mkLine [top..min (top + height) (length ls - 1)]
    e <- next_event vty
    case e of
        EvKey (KASCII 'Q') [] -> return Nothing
        EvKey (KASCII 'q') [] -> return Nothing
        EvKey KEsc []         -> return Nothing
        EvKey KDown []        -> down
        EvKey KUp []          -> up
        EvKey KHome []        -> select vty height 0 0 ls
        EvKey (KASCII ' ') [] -> choose
        EvKey KEnter []       -> choose
        EvKey KBackTab []     -> choose
        _                     -> select vty height top curr ls
        where
            choose = return . Just $ fst (ls !! curr)
            up
              | top == 0 && curr == 0 = select vty height top curr ls
              | top == curr = select vty height (top - 1) (curr - 1) ls
              | otherwise = select vty height top (curr - 1) ls
            down
              | (curr + 1) == length ls = select vty height top curr ls
              | (top + height - 1) == curr = select vty height (top + 1) (curr + 1) ls
              | otherwise = select vty height top (curr + 1) ls
            sel_attr = def_attr `with_style` standout
            mkLine n
                | n == curr = string sel_attr $ txt $ ls !! n
                | otherwise = string def_attr $ txt $ ls !! n
                where
                    txt = fst

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
    ls <- ( getArgs >>= parseArgs >>= fileLines )

    r <- withVty $ \vty -> do
        bounds <- display_bounds $ terminal vty
        let height = fromInteger (toInteger $ region_height bounds) :: Int
        select vty height 0 0 $ reverse $ freqSort ls

    case r of
        Nothing -> exitWith $ ExitFailure 1
        Just s -> write s

