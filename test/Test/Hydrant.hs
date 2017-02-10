{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Hydrant where


import           Disorder.Core
import           Disorder.Jack

import qualified Hydrant

import           P

import           System.IO (IO)

import           Test.Hydrant.Arbitrary

import qualified Text.HTML.TagSoup as TS


prop_tagsoup_equiv :: Property
prop_tagsoup_equiv =
  gamble genTagTree $ \t ->
    TS.parseTags (Hydrant.toText (tagTreeHtml t)) === treeToSoup t


treeToSoup :: TagTree -> [TS.Tag Text]
treeToSoup tt =
  case tt of
    TagNode t attrs sub ->
      fold [
          [TS.TagOpen (Hydrant.unTag t) (fmap Hydrant.unAttribute attrs)]
        , foldMap treeToSoup sub
        , [TS.TagClose (Hydrant.unTag t)]
        ]
    TagVoidNode t attrs -> [
        TS.TagOpen (Hydrant.unTag t) (fmap Hydrant.unAttribute attrs)
      , TS.TagClose (Hydrant.unTag t)
      ]
    TagText t ->
      [TS.TagText t]
    Doctype t ->
      [TS.TagComment (" DOCTYPE " <> t <> " ")]
    Comment t ->
      [TS.TagComment t]

return []
tests :: IO Bool
tests = $disorderCheckEnvAll TestRunNormal
