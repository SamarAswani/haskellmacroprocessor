module MP where

import System.Environment

type FileContents = String

type Keyword      = String
type KeywordValue = String
type KeywordDefs  = [(Keyword, KeywordValue)]

separators :: String
separators
  = " \n\t.,:;!\"\'()<>/\\"

-----------------------------------------------------
--Given a search string and a list of string/item pairs, 
--Returns the list of items whose associated string matches the search string.
lookUp :: String -> [(String, a)] -> [a]
lookUp searchString pairs
  = [y | (x,y) <- pairs, x == searchString]

--Breaks up a string into two elements in a tuple
--First element contains the separators and the second contains an array of words
splitText :: [Char] -> String -> (String, [String])
splitText _ [] = ("",[""])
splitText separators (x : xs) 
  | x `elem` separators = (x: seps, "":words)
  | otherwise = (seps, (x: head words) : tail words)
    where
       (seps, words) = splitText separators xs
       
--Combines the output of splitText into an array of strings
combine :: String -> [String] -> [String]
-- combine separators word = words
combine "" word = word
combine (sep : seps) (word : words) = word : [sep] : combine seps words


--Takes the contents of an information file in the form of a list of lines (each line is a string), and which returns a list of keyword/definition pairs.
getKeywordDefs :: [String] -> KeywordDefs
getKeywordDefs [] = []
getKeywordDefs (keyword : restKeywords) 
  = (z, concat zs) : getKeywordDefs restKeywords
  where
    (z:_:zs) =  uncurry combine (splitText " " keyword)
      
      

replaceHelper :: [String] -> KeywordDefs -> [String]
replaceHelper [] i = []
replaceHelper (word : restOfWords) i
  | word == ""       = ("" : replaceHelper restOfWords i)
  | word !! 0 == '$' = (y: replaceHelper restOfWords i)
  | otherwise        = (word: replaceHelper restOfWords i)
    where
      y = head (lookUp word i)

splitKeywords :: String -> KeywordDefs
splitKeywords i
  = getKeywordDefs y
  where 
    (x,y) = splitText "\n" i  

-- getWord :: KeywordDefs -> String -> String
-- getWord ((x,y):xs) search
--   | x == search   = y
--   | otherwise     = getWord xs search

--Takes the contents of a text file and an info file and combines them using the above functions to build a string representing the output file.
expand :: FileContents -> FileContents -> FileContents
expand t i
  = concat (combine seps s)
  where
    (seps, newText) = splitText separators t
    keywords = splitKeywords i
    s = replaceHelper newText keywords


     
  -- = splitText separators t

-- You may wish to uncomment and implement this helper function
-- when implementing expand
-- replaceWord :: String -> KeywordDefs -> String

-----------------------------------------------------

-- The provided main program which uses your functions to merge a
-- template and source file.
main :: IO ()
main = do
  args <- getArgs
  main' args

  where
    main' :: [String] -> IO ()
    main' [template, source, output] = do
      t <- readFile template
      i <- readFile source
      writeFile output (expand t i)
    main' _ = putStrLn ("Usage: runghc MP <template> <info> <output>")
