{-Evil Hangman-}
{-Author: Madison Sanchez-Forman | Version: October 27, 2021-}
import Data.List 
import Data.Ord
import qualified Data.Map as M
import Data.Function
{- Main function will start the game by prompting the user for a word length, and a number of guesses.-}
-------------------------------------------------------------------------------------
main =
  do
   putStrLn ("Word length?")
   x <- getLine
   let len = (read x) :: Int
   let lst = newDict len mediumDict -- new dict will filter out all words whose length != len 
   let startingPattern = concat (replicate len "-") 
   putStrLn ("Number of guesses?")
   n <- getLine
   let numGuesses = (read n) :: Int
   startGame lst  "" startingPattern numGuesses 
-------------------------------------------------------------------------------------
{-main recurisve game function
params: dictionary - list of words being currently used by game,
guessed - list of all previously guessed letters; any new guesses will be concatenated to this list,
pattern - the pattern the game is currently matching a guess against,
numG - number of guesses user has remaining -}
startGame dictionary guessed pattern numG
  | elem '-' pattern == False = putStrLn ("You guessed it! The word was "++ pattern) --base case 1: if the pattern no longer contains a '-', then user has won the game
  | numG == 0 = putStrLn "Game over! You are out of guesses!" --base case 2: when numG == 0, user has lost the game
  | otherwise = 
    do
      putStrLn ("You have " ++ (show numG) ++ " guesses left.")
      putStrLn ("Letters guessed: " ++ guessed)
      putStrLn ("Word: " ++ pattern)
      putStrLn ("Guess a letter: ")
      g <- getChar       --read in newest guess from user 
      let allGuesses = guessed ++"'"++[g]++"'"
      putStrLn ""
      let p = generateTuples dictionary [g] --generate pattern for each word in current dictionary
      let game = buildMap p --take list genrated from ^ and condense it to a list of tuples in the format: (pattern,[list of words matching that pattern])
      let nextPatternAndFamily = findLongest(game) --find the tuple from ^ whose list of words is the longest, this will be the next pattern and dictionary at the next iteration of the game 
      let nextPattern = fst nextPatternAndFamily --pattern
      putStrLn ""
      printBoilerplate game --print pattern matches to words
      let nextFamily = snd nextPatternAndFamily --list of words (dictionary)
      let len = length nextFamily
      putStrLn ("Using pattern " ++ nextPattern ++ ", which matches " ++ (show len) ++ " words")
      if(g `elem` nextPattern) then
        putStrLn("Good guess!") --if guess is an element of the current pattern, user has guessed correctly, if not, print incorrect
      else
        putStrLn("Sorry, there are no " ++ [g] ++ "'s") 
      if(g `elem` nextPattern) then --if guess is an element of the current pattern, user has guessed correctly and numG should not be decremented,
        startGame nextFamily allGuesses (zipPatterns pattern nextPattern) (numG)
      else
        startGame nextFamily allGuesses (zipPatterns pattern nextPattern) (numG-1) -- if the guesss was incorrect, decrement num G
-------------------------------------------------------------------------------------
{-Starting game function-}
newDict n dictionary = filter (\x -> length x == n) dictionary 
-------------------------------------------------------------------------------------
{-Functions for structuring data used in game-}
{-recursively generate a pattern for each word used based on guessed letter-}
generatePattern [g] str 
  | elem g str = [g]
  |otherwise = "-"
generatePattern (g:gs) str
  | elem g str = g : generatePattern gs str
  | otherwise = "-" ++ generatePattern gs str
{-generate a pattern for each word used in dictionary based on guessed letter, calls generatePattern-}
generateTuples [d] guess = [(generatePattern d guess, d)]
generateTuples (d:ictionary) guess = (generatePattern d guess, d):(generateTuples ictionary guess)
{-build a map(k,v) where k is a pattern and v is a list of words that match it, and then change it back into a list of tuples for functionality-}
buildMap lst =  M.toList(M.fromListWith (++) . map (\(x,y) -> (x,[y])) $ lst)
{-find the tuple in list [(k,v)] where v is longer than any other pair-}
findLongest lst = foldr1 (\a b -> if length (snd a)>length(snd b) then a else b) lst
-------------------------------------------------------------------------------------
{-Functions for printing to user -}
{-once user has made a correct guess and the pattern must be updated to include any previous correct guesses, zip them together-}
zipPatterns [] [] = []
zipPatterns (a:as) (b:bs)
  | a /= '-' = a : zipPatterns as bs
  | b /= '-' = b : zipPatterns as bs
  | otherwise = '-' : zipPatterns as bs
{-print pattern and list of words that match to it every iteration of the game-}
printBoilerplate lst = mapM_ printHelp lst
printHelp lst = putStrLn ((fst lst) ++ " matches " ++ (show (snd lst)))
-------------------------------------------------------------------------------------
-- These words match the ones used in the online writeup of the Evil Hangman
-- assignment.  You can use this dictionary to test the cases shown in the assignment.
trivialDict = ["ally", "beta", "cool", "deal", "else", "flew", "good", "hope", "ibex"]
-- The four-letter words in this dictionary contain only the letters 'e', 'a', 'l',
-- 's', 'r', and 't'.  You can take advantage of the limited character selection to
-- do some testing.
smallDict = ["alae", "alee", "ales", "area", "ares", "arse", "asea", "ates", "earl", "ears", "ease", "east", "eats", "eras", "etas", "lase", "late", "leal", "lear", "leas", "rale", "rare", "rase", "rate", "real", "rear", "sale", "sate", "seal", "sear", "seas", "seat", "sera", "seta", "tael", "tale", "tare", "tate", "teal", "tear", "teas", "tela"]
-- This is a larger group of four-letter words if you want a greater challenge.
mediumDict = ["abbe", "abed", "abet", "able", "abye", "aced", "aces", "ache", "acme", "acne", "acre", "adze", "aeon", "aero", "aery", "aged", "agee", "ager", "ages", "ague", "ahem", "aide", "ajee", "akee", "alae", "alec", "alee", "alef", "ales", "alme", "aloe", "amen", "amie", "anes", "anew", "ante", "aped", "aper", "apes", "apex", "apse", "area", "ares", "arse", "asea", "ates", "aver", "aves", "awed", "awee", "awes", "axed", "axel", "axes", "axle", "ayes", "babe", "bade", "bake", "bale", "bane", "bare", "base", "bate", "bead", "beak", "beam", "bean", "bear", "beat", "beau", "bema", "beta", "blae", "brae", "cade", "cafe", "cage", "cake", "came", "cane", "cape", "care", "case", "cate", "cave", "ceca", "dace", "dale", "dame", "dare", "date", "daze", "dead", "deaf", "deal", "dean", "dear", "deva", "each", "earl", "earn", "ears", "ease", "east", "easy", "eath", "eats", "eaux", "eave", "egad", "egal", "elan", "epha", "eras", "etas", "etna", "exam", "eyas", "eyra", "face", "fade", "fake", "fame", "fane", "fare", "fate", "faze", "feal", "fear", "feat", "feta", "flea", "frae", "gaed", "gaen", "gaes", "gage", "gale", "game", "gane", "gape", "gate", "gave", "gaze", "gear", "geta", "hade", "haed", "haem", "haen", "haes", "haet", "hake", "hale", "hame", "hare", "hate", "have", "haze", "head", "heal", "heap", "hear", "heat", "idea", "ilea", "jade", "jake", "jane", "jape", "jean", "kaes", "kale", "kame", "kane", "keas", "lace", "lade", "lake", "lame", "lane", "lase", "late", "lave", "laze", "lead", "leaf", "leak", "leal", "lean", "leap", "lear", "leas", "leva", "mabe", "mace", "made", "maes", "mage", "make", "male", "mane", "mare", "mate", "maze", "mead", "meal", "mean", "meat", "mesa", "meta", "nabe", "name", "nape", "nave", "neap", "near", "neat", "nema", "odea", "olea", "pace", "page", "pale", "pane", "pare", "pase", "pate", "pave", "peag", "peak", "peal", "pean", "pear", "peas", "peat", "plea", "race", "rage", "rake", "rale", "rare", "rase", "rate", "rave", "raze", "read", "real", "ream", "reap", "rear", "rhea", "sabe", "sade", "safe", "sage", "sake", "sale", "same", "sane", "sate", "save", "seal", "seam", "sear", "seas", "seat", "sera", "seta", "shea", "spae", "tace", "tael", "take", "tale", "tame", "tape", "tare", "tate", "teak", "teal", "team", "tear", "teas", "teat", "tela", "tepa", "thae", "toea", "twae", "urea", "uvea", "vale", "vane", "vase", "veal", "vela", "vena", "vera", "wade", "waes", "wage", "wake", "wale", "wame", "wane", "ware", "wave", "weak", "weal", "wean", "wear", "weka", "yare", "yeah", "yean", "year", "yeas", "zeal", "zeta", "zoea"]
-- The full dictionary has over 120,000 words.  It's pretty much impossible to win
-- against the Evil Hangman algorithm with a dictionary this large.
