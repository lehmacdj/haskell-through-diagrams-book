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
mkLink is s = ((\i -> paddedNumber i ++ "-") =<< is) ++ linkText s ++ ".md"
reference i = "[" ++ show i ++ "]"
makePrologue s = "[" ++ s ++ "]" ++ "(" ++ link (PQ [] s) ++ ")"

header = ["# Summary", ""]
title s = "# " ++ s
skeletonPrefix = "./src/"

generatePrologue :: String -> IO ()
generatePrologue s = do
    let link = mkLink [] s
        file = skeletonPrefix ++ link
    exists <- doesFileExist file
    unless exists $ writeFile file $ title s

link :: PageQual -> String
link (PQ is s) = mkLink is s

data PageQual = PQ [Int] String

generatePage pq@(PQ is s) = do
    let file = skeletonPrefix ++ link pq
    exists <- doesFileExist file
    unless exists $ writeFile file $ title s

chapterize :: [String] -> [Int -> [PageQual]]
chapterize [] = []
chapterize (x:xs) = (\i -> PQ [i] x:makeSub i xs') : chapterize ys' where
    (xs', ys') = span ("    " `isPrefixOf`) xs
    makeSub i = zipWith (\x s -> PQ [i, x] (drop 4 s)) [1..]

makeBullet' :: PageQual -> String
makeBullet' pq@(PQ is s) = prefix ++ "- [" ++ s ++ "]" ++ "(" ++ link pq ++ ")"
    where prefix = replicate (4 * (length is - 1)) ' '

main :: IO ()
main = do
    index <- readFile "chapter-index"
    let entries = filter (not . ("#" `isPrefixOf`)) $ lines index
        pEntries = takeWhile (not . null) entries
        cEntries = drop 1 $ dropWhile (/="") entries
        cEntries' = concat $ zipWith ($) (chapterize cEntries) [1..]
        indexes = [1..]
        prologue = map makePrologue pEntries
        chapters = map makeBullet' cEntries'
        skeletons = map generatePrologue pEntries ++ map generatePage cEntries'
    writeFile "src/SUMMARY.md" $ unlines $
        header ++ prologue ++ [""] ++ chapters
    sequence_ skeletons
