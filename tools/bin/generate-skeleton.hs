import System.IO
import Data.Char (toLower, isAlphaNum)
import Data.List (intercalate, isPrefixOf)
import Control.Monad
import System.Directory

paddedNumber :: Int -> String
paddedNumber n
  | n < 10 = "0" ++ show n
  | otherwise = show n

linkText s = intercalate "-" $ filter isAlphaNum . map toLower <$> words s
qualifiedLink i s = paddedNumber i ++ "-" ++ linkText s ++ ".md"
unqualifiedLink s = linkText s ++ ".md"
reference i = "[" ++ show i ++ "]"
makeBullet i s = "- [" ++ s ++ "]" ++ "(" ++ qualifiedLink i s ++ ")"
makePrologue s = "[" ++ s ++ "]" ++ "(" ++ unqualifiedLink s ++ ")"

header = ["# Summary", ""]
title s = "# " ++ s
skeletonPrefix = "./src/"

generatePrologue :: String -> IO ()
generatePrologue s = do
    let link = unqualifiedLink s
        file = skeletonPrefix ++ link
    exists <- doesFileExist file
    unless exists $ writeFile file $ title s

generateChapter :: Int -> String -> IO ()
generateChapter i s = do
    let link = qualifiedLink i s
        file = skeletonPrefix ++ link
    exists <- doesFileExist file
    unless exists $ writeFile file $ title s

main :: IO ()
main = do
    index <- readFile "chapter-index"
    let entries = lines index
        pEntries' = takeWhile (not . null) entries
        cEntries' = drop 1 $ dropWhile (/="") entries
        pEntries = filter (not . ("#" `isPrefixOf`)) pEntries'
        cEntries = filter (not . ("#" `isPrefixOf`)) cEntries'
        indexes = [1..]
        prologue = map makePrologue pEntries
        chapters = zipWith makeBullet indexes cEntries
        skeletons = map generatePrologue pEntries
            ++ zipWith generateChapter indexes cEntries
    writeFile "src/SUMMARY.md" $ unlines $
        header ++ prologue ++ [""] ++ chapters
    sequence_ skeletons
