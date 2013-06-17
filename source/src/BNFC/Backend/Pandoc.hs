{-# LANGUAGE OverloadedStrings #-}
module BNFC.Backend.Pandoc (makePandoc) where

import BNFC.Options
import BNFC.CF
import Text.Pandoc.Builder
import System.FilePath ((<.>),replaceExtension)

makePandoc :: SharedOptions -> String -> CF -> IO ()
makePandoc opts _ cf = do
    let texfile = name <.> "tex"
    writeFile texfile (show $ cf2Pandoc cf name)
  where name = lang opts

cf2Pandoc :: CF -> String -> Pandoc
cf2Pandoc cf language = setTitle title $ setAuthors [author] $ doc $
  -- introduction
  para ( "This document was automatically generated by the"
      <> emph "BNF-Converter."
      <> "It was generated together with the lexer, the parser, and the"
      <> "abstract syntax module, which guarantees that the document"
      <> "matches with the implementation of the language (provided no"
      <> "hand-hacking has taken place)." )

  header 2 ("The lexical structure of " ++ language)
    ( -- if not (hasIdent cf) then [] else
      header 3 "Identifiers" (
        para ( "Identifiers" <> emph "Ident"
            <> "are unquoted strings beginning with a letter,"
            <> "followed by any combination of letters, digits,"
            <> "and the characters ``_ '``, reserved words excluded." ))
   <> header 3 "Literals"
        mconcat
              [ char | "Char" `elem` literals cf ]


-- Lexical structure of the language
sLexical :: String Blocks
sLexical language =
    header 2 ("The lexical structure of " ++ language) $
      ssIdentifiers <> ssLiterals

ssIdentifiers = header 3 "Identifiers" $ para $
  "Identifiers" <> emph "Ident" <> "are unquoted strings beginning with"
  <> "a letter, followed by any combination of letters, digits,"
  <> "and the characters ``_ '``, reserved words excluded."

ssLiterals = header 3 "Literals" $ mconcat $
     [para ("Character literals" <> emph "Char" <> "have the form")]
  ++ 
      



        unlines $ map stringLit $
  para ( "Identifiers" <> emph "Ident"        filter (`notElem` ["Ident"]) $
            literals cf
stringLit cat = unlines $ case cat of
  "Char" -> ["Character literals //Char// have the form",
             "``'``//c//``'``, where //c// is any single character.",
             ""
            ]
  "String" -> ["Sheader 2 ("The lexical structure of " ++ language)tring literals //String// have the form",
             "``\"``//x//``\"``}, where //x// is any sequence of any characters",
             "except ``\"`` unless preceded by ``\\``.",
             ""]
  "Integer" -> ["Integer literals //Integer// are nonempty sequences of digits.",
             ""]
  "Double" -> ["Double-precision float literals //Double// have the structure",
               "indicated by the regular expression" +++
               "``digit+ '.' digit+ ('e' ('-')? digit+)?`` i.e.\\",
               "two sequences of digits separated by a decimal point, optionally",
               "followed by an unsigned or negative exponent.",
         "and the characters ``_ '``, reserved words excluded."      ""]
  _ -> []


       ( 
                               prtLiterals name cf,
                               unlines (map prtOwnToken (tokenPragmas cf)),
			       "===Reserved words and symbols===",
			       prtReserved name cf,
			       prtSymb name cf,
			       "===Comments===",
			       prtComments $ comments cf
			    prtBNF name cf,
		     
                     
                     
                     para "This is the first paragraph"
  <> para ("And " <> emph "another" <> ".")
  <> bulletList
      [ para "item one" <> para "continuation"
      , plain ("item two and a " <> link "/url" "go to url" "link") ]

  where title   = "The " ++ language ++ " Language"
        author  = "The BNF converter"
