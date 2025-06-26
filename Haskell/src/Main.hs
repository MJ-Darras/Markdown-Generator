module Assignment (markdownParser, convertADTHTML) where

import           Data.Time.Clock  (getCurrentTime)
import           Data.Time.Format (defaultTimeLocale, formatTime)
import           Instances        (Parser (..))
import           Parser           
import Control.Applicative 
import System.IO

data ADT = Italic String
  | Bold String
  | Strikethrough String
  | Link String String
  | InlineCode String 
  | FootNote Int
  | FootNoteReference String Int
  | Image String String String
  | TextLine [ADT]
  | Text String 
  | Heading Int [ADT]
  | Blockquote [ADT]
  | Code String [Char]
  | OrderedList [ADT]
  | InnerList [ADT]
  | Table [ADT] [[ADT]]
  | Cell ADT
  | NewLine 
  | MarkdownList [ADT]  
  | Regular String
  deriving (Show, Eq)

-- | Code String [ADT]
  -- | Separator [String]


-- textParser :: Parser String
--parses words till the first space
-- textParser = some (alpha <|> is '.')  
-- many stringTok  --this will be adjusted as needed later (Might make an AlphaNum parser)

modifiedtextParser:: Parser String
modifiedtextParser = some (noneof "\n*~ ") --parses text only

-- regTextParser:: Parser String
-- regTextParser = some (noneof "\n")

-- punctuationParser:: Parser Char
-- punctuationParser = oneof "@#$%^&<>,.!?;:'"

-- punctuationParser:: Parser Char
-- punctuationParser = noneof "\n*_`~"

modifierSymbols:: String
modifierSymbols = "*_`~"

-- sentenceParser:: Parser String                  
-- sentenceParser = do                             
--   text <- some (alpha <|> punctuationParser <|> oneof "\t\r\f\v " <|> digit)  --parser sentences made of characters/spaces
--   return $ text

-- sentenceParser2:: Parser String                  
-- sentenceParser2 = do                             
--   text <- some (punctuationParser)  --parser sentences made of characters/spaces
--   return $ text

stringSections:: Char -> Parser String
stringSections inputChar = concat <$> some (fmap (:[]) (noneof (inputChar : "\n")) <|> complexParser inputChar)
  where
  complexParser inputChara = do
        _           <- is inputChara
        secondPart  <- modifiedtextParser
        return $ inputChara : secondPart

urlParser:: Parser String
urlParser = do
  _   <- inlineSpace
  url <- some (noneof ")\t\r\f\v ")   --parses spaces after the url
  _   <- inlineSpace                  --makes it so that the parser expects spaces
  return url

positiveIntParser :: Parser Int
positiveIntParser = do 
  number <-  int               
  if number > 0
    then return number
    else empty 

freeTextParser:: Parser ADT
freeTextParser = do 
  _ <- inlineSpace
  elements <- some (freeTextParserHelper)
  return $ TextLine (concat elements)

freeTextParserHelper:: Parser [ADT]
freeTextParserHelper = do 
  element <- modifierParser <|> (Text <$> some (noneof (" " ++ modifierSymbols)))
  someSpaces <- spaces
  next <- freeTextParserHelper <|> pure []
  return $ element : (if null next then [] else (Text someSpaces) : next)

-- freeTextParser:: Parser ADT
-- freeTextParser = do 
--   _ <- inlineSpace
--   elements <- some (modifierParser <|> freeTextParserHelper)
--   return $ TextLine elements

-- freeTextParserHelper:: Parser ADT
-- freeTextParserHelper = do 
--   text <- some (noneof (" " ++ modifierSymbols))
--   someSpaces <- spaces
--   next <- freeTextParserHelper <|> pure (Text "")
--   let result = if next == (Text "")
--                then text
--                else text ++ someSpaces ++ (extractText next) 
--   return $ Text result

-- textLineParser:: Parser ADT
-- textLineParser = do
--   line <- some (freeTextParser <|> modifierParser)
--   return $ TextLine line

modifierParser:: Parser ADT
modifierParser = italicsParser <|> boldParser <|> strikethroughParser <|> linkParser 
                 <|> inlineCodeParser <|> footNoteParser

newLineParser:: Parser ADT
newLineParser = do 
  _ <- is '\n'
  return NewLine

-- ================================

italicsParser :: Parser ADT
italicsParser = do 
  _             <- is '_'
  text  <- some (noneof "\n_")
  _             <- is '_'
  return $ Italic text

boldParser :: Parser ADT
boldParser = Bold <$> (string "**" *> stringSections '*' <* string "**")

strikethroughParser :: Parser ADT
strikethroughParser = Strikethrough <$> (string "~~" *> stringSections '~' <* string "~~")

linkParser :: Parser ADT
linkParser = do
  _ <- is '['
  link_text <- some (noneof "\n]")
  _ <- is ']'
  _ <- inlineSpace
  _ <- is '('
  url <- inlineSpace *> some (noneof ")\t\r\f\v\n ") <* inlineSpace 
  _ <- is ')' <* inlineSpace
  return $ Link link_text url

inlineCodeParser :: Parser ADT
inlineCodeParser = do
  _ <- is '`'
  code <- some (noneof "\n`")
  _ <- is '`'
  return $ InlineCode code

footNoteParser :: Parser ADT
footNoteParser = do
  _   <- string "[^"
  n <- positiveIntParser                  --[^ℤ+], where ℤ+ = {1,2,3,…}, i.e., any positive integer
  _   <- is ']' <* inlineSpace
  return $ FootNote n

-- ================================

imageParser:: Parser ADT
imageParser = do
  _ <- inlineSpace
  _ <- is '!'
  _ <- is '['
  altTxt <- some (noneof "]\n")
  _ <- is ']'
  _ <- inlineSpace
  _ <- is '('
  _ <- inlineSpace
  url <- some (noneof ")\t\r\f\v ") 
  _ <- inlineSpace
  captionTxt <-  (string "\"" *> some (noneof "\"\t\r\f\v") <* inlineSpace <* string "\"")
  _ <- inlineSpace *> is ')'
  return $ Image altTxt url captionTxt

footnoteSentenceParser :: Parser String
footnoteSentenceParser = some (noneof "\n")

footNoteReferenceParser:: Parser ADT
footNoteReferenceParser = do
  _ <- inlineSpace
  fn <- footNoteParser
  _ <- is ':'
  text <- inlineSpace *> footnoteSentenceParser <* inlineSpace
  case fn of
    FootNote n -> return $ FootNoteReference text n
    _ -> empty 
  
-- Headings can have footnote but not the references

heading0Parser:: Parser ADT
heading0Parser = do
  _ <- inlineSpace
  hashes <- some (is '#')
  _ <- spaces1
  let value = length hashes
  if value > 6
    then empty
    else do 
      content <- some (modifierParser <|> freeTextParser)
      return $ Heading value content

altHeading1Parser:: Parser ADT
altHeading1Parser = do
  _ <- inlineSpace
  content <- some (modifierParser <|> freeTextParser)
  _ <- is '\n'
  _ <- inlineSpace
  _ <- is '='
  _ <- some (is '=') <* eof
  return $ Heading 1 content

altHeading2Parser:: Parser ADT
altHeading2Parser = do
  _ <- inlineSpace
  content <- some (modifierParser <|> freeTextParser)
  _ <- is '\n'
  _ <- inlineSpace
  _ <- is '-'
  _ <- some (is '-')
  return $ Heading 2 content

finalHeadingParser:: Parser ADT
finalHeadingParser = heading0Parser <|> altHeading1Parser <|> altHeading2Parser

-- blockquotesParser:: Parser ADT
-- blockquotesParser = do  
--   _ <- inlineSpace *> is '>' *> inlineSpace
--   text <- many ((Text <$> some (noneof ("\n" ++ modifierSymbols))) <|> modifierParser) -- <|> blockquotesParser)
--   return $ Blockquote text

blockquotesParser:: Parser ADT
blockquotesParser = do  
  quotes <- some (blockquoteHelper) <* inlineSpace
  return $ Blockquote quotes


blockquoteHelper:: Parser ADT
blockquoteHelper = do  
  _ <- inlineSpace *> is '>' *> inlineSpace
  text <- many ((Text <$> some (noneof ("\n" ++ "*_`~>"))) <|> modifierParser) -- <|> blockquotesParser)
  _ <- many (is '\n')
  return $ TextLine text

-- codeParser:: Parser ADT
-- codeParser = do
--   _ <- inlineSpace *> string "```"
--   language <- many (noneof "\n ") <* inlineSpace
--   _ <- many (is '\n')
--   text <- many (freeTextParser <* newLineParser)
--   _ <- string "```"
--   return $ Code language text 

codeParser:: Parser ADT
codeParser = do
  _ <- inlineSpace *> string "```"
  language <- inlineSpace *> many (noneof "\n ") <* inlineSpace
  text <- many (noneof "`")
  _ <- string "```"
  return $ Code language text

-- ================================

orderedList:: Parser ADT
orderedList = do
  _ <- string "1." <* some (oneof "\t\r\f\v ")
  item <- modifierParser <|> (Regular <$> many (noneof "\n")) 
  restItems <- many (orderedListHelper <|> subList) 
  return $ OrderedList (item : restItems)

orderedListHelper:: Parser ADT
orderedListHelper = do
  _ <- inlineSpace *> is '\n'
  _ <- digit <* is '.' <* some (oneof "\t\r\f\v ")
  sndItem <- modifierParser <|> (Regular <$> many (noneof "\n") ) 
  return $ sndItem

subList:: Parser ADT
subList = do
  _ <- inlineSpace *> is '\n'
  _ <-  string "    " <* string "1." <* some (oneof "\t\r\f\v ") --(oneof "\t")
  item <- modifierParser <|> (Regular <$> many (noneof "\n") ) 
  restItems <- many subListHelper 
  return $ InnerList (item : restItems)

subListHelper:: Parser ADT
subListHelper = do
  _ <- inlineSpace *> is '\n'
  _ <- string "    " <* digit <* is '.' <* some (oneof "\t\r\f\v ")
  sndItem <- modifierParser <|> (Regular <$> many (noneof "\n") ) 
  return $ sndItem

-- ================================

tableParser:: Parser ADT
tableParser = do
  _ <- inlineSpace
  header <- tableRowParser
  let colCount = length header
  _ <- inlineSpace *> headerSeparatorParser colCount
  rows <- many (rowOfLength colCount)
  return $ Table header rows

-- headerSeparatorParser:: Int -> Parser ADT
-- headerSeparatorParser colCount = do
--   _ <- inlineSpace *> is '|' 
--   separators <- some (inlineSpace *> string "---" <* many (is '-') <* inlineSpace <* is '|')
--   _ <- is '\n' --inlineSpace *> 
--   if length separators == colCount
--     then return $ Separator separators
--     else empty

headerSeparatorParser:: Int -> Parser [String]
headerSeparatorParser colCount = do
  _ <- inlineSpace *> is '|' 
  separators <- some (inlineSpace *> string "---" <* many (is '-') <* inlineSpace <* is '|')
  _ <- is '\n' --inlineSpace *> 
  if length separators == colCount
    then return $ separators
    else empty

rowOfLength:: Int -> Parser [ADT]
rowOfLength colCount = do 
  row <- tableRowParser
  if length row == colCount
    then return row
    else empty  

tableRowParser:: Parser [ADT]
tableRowParser = do 
  _ <- inlineSpace *> is '|'
  cells <- some (cellParser <* (inlineSpace *> is '|' <* inlineSpace))
  _ <- optional(is '\n')
  return cells

cellParser:: Parser ADT
cellParser = do
  content <- ((inlineSpace *> modifierParser <* inlineSpace) <|>  Regular <$> (cellTextParser)) 
  return $ Cell content

cellTextParser:: Parser String
cellTextParser = do 
  _ <- inlineSpace
  elements <- some cellTextParserHelper
  return $ concat elements

cellTextParserHelper:: Parser String
cellTextParserHelper = do 
  element <- some (noneof ("\n| " ++ modifierSymbols))
  someSpaces <- inlineSpace
  next <- cellTextParserHelper <|> pure ""
  return $ element ++ (if null next then "" else someSpaces ++ next)

-- ================================

-- returns none or more
markdownParser :: Parser ADT
markdownParser = inlineSpace *> (MarkdownList <$> many innerParser) <* inlineSpace
      where 
        innerParser =  finalHeadingParser <|> tableParser <|> orderedList <|> codeParser
                      <|> blockquotesParser <|> footNoteReferenceParser <|> imageParser
                      <|> italicsParser <|> boldParser <|> strikethroughParser <|> linkParser 
                      <|> inlineCodeParser <|> footNoteParser 
                      <|> freeTextParser <|> newLineParser

getTime :: IO String
getTime = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" <$> getCurrentTime

-- =========================================================================

indent :: Int -> String -> String
indent level str = replicate (level * 4) ' ' ++ str

convertADTHTML :: ADT -> String
convertADTHTML adt = unlines $ headerHTML ++ (convertHelper adt 0) ++ footerHTML
  where 
    headerHTML = ["<!DOCTYPE html>"
             ,"<html lang=\"en\">\n"
             ,"<head>"
             ,indent 1 "<meta charset=\"UTF-8\">"
             ,indent 1 "<title>Test</title>"
             ,"</head>\n\n"
             ,"<body>"]
    footerHTML = ["</body>\n", "</html>"]


convertHelper:: ADT -> Int -> [String]
convertHelper (Italic text) indentLevel = 
  [indent indentLevel ("<em>" ++ text ++ "</em>")]

convertHelper (Bold text) indentLevel = 
  [indent indentLevel ("<strong>" ++ text ++ "</strong>")]

convertHelper (Strikethrough text) indentLevel = 
  [indent indentLevel ("<del>" ++ text ++ "</del>")]

convertHelper (Link text url) indentLevel = 
  [indent indentLevel ("<a href=\"" ++ url ++ "\">" ++ text ++ "</a>")]

convertHelper (InlineCode code) indentLevel = 
  [indent indentLevel ("<code>" ++ code ++ "</code>")]

convertHelper (FootNote n) indentLevel = 
  [indent indentLevel ("<sup><a id=\"fn" ++ show n ++ "ref\" href=\"#fn" ++ show n ++ "\">" ++ show n ++ "</a></sup>")]

convertHelper (FootNoteReference text n) indentLevel = 
  [indent indentLevel ("<p id=\"fn" ++ show n ++ "\">" ++ text ++ "</p>")]

convertHelper (Image altTxt url caption) indentLevel = 
  [indent indentLevel ("<img src=\"" ++ url ++ "\" alt=\"" ++ altTxt ++ "\" title=\"" ++ caption ++ "\">")]

convertHelper (Regular text) indentLevel = 
  [indent indentLevel (text)]

-- convertHelper (TextLine text) indentLevel = 
--   [indent indentLevel ("<p>" ++
--   concat (concatMap (`convertHelper` indentLevel) text) ++ 
--   "</p>")]

convertHelper (TextLine text) indentLevel = 
  [indent indentLevel ("<p>" ++
  concatMap (processContent 0) text ++ 
  "</p>")]

convertHelper (Text text) indentLevel = 
  [indent indentLevel ("<p>" ++ text ++ "</p>")]

-- convertHelper (Text text) indentLevel = 
--   [indent indentLevel (text)]

convertHelper (Heading level content) indentLevel = 
  [indent indentLevel ("<h" ++ show level ++ ">" ++ 
  concatMap (processContent indentLevel) content ++ 
  "</h" ++ show level ++ ">")]

-- might need unlines
convertHelper (Blockquote content)   indentLevel = 
  [indent indentLevel "<blockquote>"] ++ 
  concatMap (`convertHelper` (indentLevel + 1)) content ++
  [indent indentLevel "</blockquote>"]

-- convertHelper (Code language content)    indentLevel = 
--     [indent indentLevel ("<pre><code class=\"language-" ++ language 
--     ++ "\">" ++ unlines (map extractText content) ++ "</code></pre>")] 

convertHelper (Code language content)    indentLevel = 
    [indent indentLevel ("<pre><code class=\"language-" ++ language 
    ++ "\">" ++ content ++ "</code></pre>")] 

-- convertListItems
convertHelper (OrderedList items) indentLevel = 
    [indent indentLevel "<ol>"] ++ 
    concatMap (`convertListItems` (indentLevel + 1)) items ++ 
    [indent (indentLevel + 1) "</li>"] ++ [indent indentLevel "</ol>"]

convertHelper (InnerList items) indentLevel = 
    [indent indentLevel "<ol>"] ++ 
    concatMap (`convertListItems` (indentLevel + 1)) items ++ 
    [indent indentLevel "</ol>"] ++  [indent (indentLevel - 1) "</li>"]

-- tableRow might need unlines
convertHelper (Table headers rows) indentLevel =
    [indent indentLevel "<table>"] ++ 
    [indent (indentLevel + 1) "<thead>"] ++ tableHeadingRow headers (indentLevel + 2) ++ 
    [indent (indentLevel + 1) "</thead>"] ++ 
    [indent (indentLevel + 1) "<tbody>"] ++ concatMap (`tableRow` (indentLevel + 2)) rows ++ 
    [indent (indentLevel + 1) "</tbody>"] ++ 
    [indent indentLevel "</table>"]

-- convertHelper NewLine _ = ["<br>"]
convertHelper NewLine _ = empty

convertHelper (MarkdownList items) indentLevel =
  concatMap (\adt -> convertHelper adt indentLevel) items

convertHelper (Cell content) indentLevel = 
  [indent indentLevel "<td>"] ++ convertHelper content (indentLevel + 1) ++ [indent indentLevel "</td>"]

-- =================================================================================

convertInline :: [ADT] -> String
convertInline = concatMap (head . (`convertHelper` 0))

tableRow :: [ADT] -> Int -> [String]
tableRow cells indentLevel = 
    [indent indentLevel "<tr>"] ++ concatMap (`convertCell` (indentLevel + 1)) cells ++ [indent indentLevel "</tr>"]

tableHeadingRow :: [ADT] -> Int -> [String]
tableHeadingRow cells indentLevel = 
    [indent indentLevel "<tr>"] ++ concatMap (`convertHeading` (indentLevel + 1)) cells ++ [indent indentLevel "</tr>"]

convertCell :: ADT -> Int -> [String]
convertCell (Cell content) indentLevel = do
  let convertedContent = concat $ convertHelper content 0
  [indent indentLevel "<td>" ++ convertedContent ++  "</td>"]
convertCell _ _ = []

convertHeading :: ADT -> Int -> [String]
convertHeading (Cell content) indentLevel = do
  let convertedContent = concat $ convertHelper content 0 
  [indent indentLevel "<th>" ++ convertedContent ++ "</th>"]
convertHeading _ _ = []

-- convertListItems :: ADT -> Int -> [String]
-- convertListItems (Regular item) indentLevel = 
--   [indent indentLevel "<li>"] ++ convertHelper (Regular item) (indentLevel + 1) ++ [indent indentLevel "</li>"]
-- convertListItems other indentLevel = convertHelper other (indentLevel + 1) 

convertListItems :: ADT -> Int -> [String]
-- convertListItems (Regular item) indentLevel = 
--   [indent indentLevel "<li>"] ++ convertHelper (Regular item) (indentLevel + 1)
convertListItems (InnerList items) indentLevel = convertHelper (InnerList items) (indentLevel + 1) 
convertListItems other indentLevel = [indent indentLevel "<li>"] ++ 
                                     convertHelper other (indentLevel + 1)


processContent:: Int -> ADT -> String
processContent _ (Text t) = extractText (Text t)
processContent indentLevel adt = concat $ convertHelper adt indentLevel

extractText :: ADT -> String
extractText (Regular t) = t
extractText (Text t) = t
extractText (Italic t) = t
extractText (Bold t) = t
extractText (Strikethrough t) = t
extractText (Link t url) = t
extractText (InlineCode t) = t
extractText (TextLine items) = concatMap extractText items
extractText _ = ""