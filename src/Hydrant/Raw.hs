{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Hydrant.Raw (
  -- * Elements
    textNode
  , textNodeUnescaped
  , parentNode
  , voidNode
  , tagOpen
  , tagClose
  , comment
  -- * Escaping
  , escapeEntities
  ) where


import           Data.Foldable (Foldable (..))
import           Data.Functor (Functor(..))
import           Data.Function ((.))
import qualified Data.List as L
import           Data.Monoid ((<>))
import           Data.Tuple (uncurry)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as TLB


textNode :: Text -> Builder
textNode =
  TLB.fromText . escapeEntities

textNodeUnescaped :: Text -> Builder
textNodeUnescaped =
  TLB.fromText

parentNode :: Text -> [(Text, Text)] -> Builder -> Builder
parentNode tag attrs b =
  tagOpen tag attrs <> b <> tagClose tag

voidNode :: Text -> [(Text, Text)] -> Builder
voidNode tag attrs =
     "<"
  <> fold (L.intersperse " " (TLB.fromText (escapeEntities tag) : fmap (uncurry attr) attrs))
  <> "/>"

tagOpen :: Text -> [(Text, Text)] -> Builder
tagOpen tag attrs =
     "<"
  <> fold (L.intersperse " " (TLB.fromText (escapeEntities tag) : fmap (uncurry attr) attrs))
  <> ">"

tagClose :: Text -> Builder
tagClose t =
  TLB.fromText ("</" <> t <> ">")

attr :: Text -> Text -> Builder
attr key val =
  TLB.fromText key <> TLB.fromText "=\"" <> TLB.fromText (escapeEntities val) <> TLB.fromText "\""

-- | Comment text is not escaped. The user must ensure it satisfies their chosen HTML standard.
--
-- e.g. for HTML 5:
--
-- * MUST NOT contain @--@.
--
-- * MUST NOT start with @>@
--
-- * MUST NOT start with @->@
comment :: Text -> Builder
comment t =
  "<!--" <> TLB.fromText t <> "-->"

-- -----------------------------------------------------------------------------
-- Escaping

-- | Performs minimal entity escaping as follows:
--
-- > case c of
-- >   '<'  -> "&lt;"
-- >   '>'  -> "&gt;"
-- >   '&'  -> "&amp;"
-- >   '"'  -> "&quot;"
-- >   '\'' -> "&#39;"
-- >   x    -> fromString [x]
escapeEntities :: Text -> Text
escapeEntities =
    T.replace "<" "&lt;"
  . T.replace ">" "&gt;"
  . T.replace "&" "&amp;"
  . T.replace "\"" "&quot;"
  . T.replace "'" "&#39;"
{-# INLINE escapeEntities #-}
