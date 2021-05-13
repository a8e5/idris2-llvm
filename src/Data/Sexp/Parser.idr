module Data.Sexp.Parser

import Libraries.Text.Bounded
import Libraries.Text.Parser

import Data.List1
import Data.Sexp
import Data.Sexp.Lexer

Parser : Type -> Type -> Type
Parser tok a = Grammar () tok True a

exact : Eq a => Show a => a -> Parser a ()
exact t = terminal ("expected " ++ (show t)) f where
  f : a -> Maybe ()
  f x = if x == t then Just () else Nothing

ignoreComment : Parser Token ()
ignoreComment = terminal "comment" f where
  f : Token -> Maybe ()
  f (Comment _) = Just ()
  f _ = Nothing

mutual
  oneSexp : Parser Token Sexp
  oneSexp = (many ignoreComment) *> (parseSAtom <|> parseSList)

  parseSList : Parser Token Sexp
  parseSList = do
    content <- ((exact LParen) `seq` \_ => many oneSexp <* (exact RParen))
    pure $ SList content

  parseSAtom : Parser Token Sexp
  parseSAtom = terminal "atom" isAtom where
    isAtom : Token -> Maybe Sexp
    isAtom (Atom a) = Just $ SAtom a
    isAtom (QuotedAtom a) = Just $ SAtom a
    isAtom _ = Nothing

sexpMain : Parser Token (List Sexp)
sexpMain = forget <$> some oneSexp

export
parseSexp : List (WithBounds Token) -> Either (String) (List Sexp)
parseSexp toks = case parse sexpMain toks of
                      Left (Error s remaining) => Left $ "parse error: \"" ++ s ++ "\"\nremaining tokens: " ++ (show "xxx")
                      Right (e, toks) => Right e
