{-# LANGUAGE TemplateHaskell #-}
module BNFC.Template where

import Language.Haskell.TH

type Template = [Exp]

data TemplateP = StringT String | VariableT Name
  deriving (Eq,Show)

template :: FilePath -> Q Exp
template f = do
  txt <- runIO (readFile f)
  emptyString  <- [| "" |]
  stringConcat <- [| (++) |]
  let concatExp e1 e2 = AppE (AppE stringConcat e1) e2
  return $ foldl concatExp emptyString (parseTemplate txt)

parseTemplate :: String -> Template
parseTemplate = s []
  where s [] [] = []
        s as [] = [LitE (StringL (reverse as))]
        s [] ('#':'{':cs) = c [] cs
        s as ('#':'{':cs) = LitE (StringL (reverse as)) : c [] cs
        s as (c:cs) = s (c:as) cs
        c [] [] = error "Unfinished code in template"
        c as ('}':cs) = VarE (mkName (reverse as)) : s [] cs
        c as (x:cs) = c (x:as) cs
