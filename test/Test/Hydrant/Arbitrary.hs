{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Hydrant.Arbitrary where


import qualified Data.Text as T

import Disorder.Core (genValidUtf81)
import Disorder.Corpus
import Disorder.Jack

import Hydrant

import P


-- structured representation for testing purposes
data TagTree
  = TagNode Tag [Attribute] [TagTree]
  | TagVoidNode Tag [Attribute]
  | TagText Text
  | Doctype Text
  | Comment Text
  deriving (Eq, Show)

genTagTree :: Jack TagTree
genTagTree =
  sized genTagTree'

genTagTree' :: Int -> Jack TagTree
genTagTree' k
  | k <= 2 = oneOf [genTxt, genComment, genDoctype]
  | k <= 10 = oneOf [genVoid k, genNode k]
  | otherwise = genNode k
  where
    genAttr = Attribute <$> genAttributeKey <*> genAttributeValue
    genTxt = fmap TagText genUtf81
    genComment = fmap Comment genValidComment
    genDoctype = fmap Doctype genValidDoctype
    genVoid x =
      TagVoidNode
        <$> genTag
        <*> vectorOf (max 0 (x - 1)) genAttr
    genNode x = do
      t <- genTag
      nats <- chooseInt (0, max 0 (x-1))
      attrs <- vectorOf nats genAttr
      let x' = max 0 (x - nats - 1)
      (TagNode t attrs . merge) <$> subtree x' []

subtree :: Int -> [TagTree] -> Jack [TagTree]
subtree 0 acc = pure acc
subtree k acc = do
  j <- chooseInt (1, k)
  t <- genTagTree' j
  subtree (k - j) (t:acc)

merge :: [TagTree] -> [TagTree]
merge tt =
  case tt of
    (TagText a : TagText b : ttt) ->
      merge (TagText (a <> b) : ttt)
    (a : bs) ->
      a : merge bs
    [] ->
      []

genTag :: Jack Tag
genTag =
  fmap Tag (elements muppets)

genAttributeKey :: Jack AttributeKey
genAttributeKey =
  fmap AttributeKey (elements simpsons)

genAttributeValue :: Jack AttributeValue
genAttributeValue =
  fmap AttributeValue $ oneOf [
      elements viruses
    , genUtf81
    ]

tagTreeHtml :: TagTree -> Html
tagTreeHtml (TagNode t a ts) =
  parentNode t a (foldMap tagTreeHtml ts)
tagTreeHtml (TagVoidNode t a) =
  voidNode t a
tagTreeHtml (TagText t) =
  textNode t
tagTreeHtml (Doctype t) =
  doctype t
tagTreeHtml (Comment t) =
  comment t

genHtml :: Jack Html
genHtml =
  fmap tagTreeHtml genTagTree

genUtf81 :: Jack Text
genUtf81 =
  mkJack shrinkText genValidUtf81

-- just shrink from both sides
shrinkText :: Text -> [Text]
shrinkText t
  | T.length t <= 1 = []
  | T.length t == 2 =
      case T.unpack t of
        (a:b:[]) ->
          [T.singleton a, T.singleton b]
        _ ->
          []
  | otherwise =
      let (heads, tails) = (T.drop 1 t, T.dropEnd 1 t) in
      fold [[heads, tails], shrinkText heads, shrinkText tails]

-- From W3C HTML 5 Recommendation Section 8.1.6:
--
-- Comments must start with the four character sequence U+003C
-- LESS-THAN SIGN, U+0021 EXCLAMATION MARK, U+002D HYPHEN-MINUS,
-- U+002D HYPHEN-MINUS (<!--). Following this sequence, the comment
-- may have text, with the additional restriction that the text must
-- not start with a single ">" (U+003E) character, nor start with a
-- U+002D HYPHEN-MINUS character (-) followed by a ">" (U+003E)
-- character, nor contain two consecutive U+002D HYPHEN-MINUS
-- characters (--), nor end with a U+002D HYPHEN-MINUS character
-- (-). Finally, the comment must be ended by the three character
-- sequence U+002D HYPHEN-MINUS, U+002D HYPHEN-MINUS, U+003E
-- GREATER-THAN SIGN (-->).
genValidComment :: Jack Text
genValidComment =
  fmap (T.replace "-" "\\-") . suchThat genUtf81 $ \t ->
    and [
        T.take 1 t /= ">"
      , T.take 2 t /= "->"
      , T.takeEnd 1 t /= "-"
      ]

genValidDoctype :: Jack Text
genValidDoctype =
  fmap (T.filter (/= '>')) genValidComment
