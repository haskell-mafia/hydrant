{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Hydrant (
  -- * Rendering
    toLazyText
  , toText
  , toUtf8Builder
  -- * Elements
  , Html
  , Tag
  , Attribute
  , AttributeKey
  , AttributeValue
  , textNode
  , textNodeUnescaped
  , parentNode
  , voidNode
  -- * Escaping
  , Raw.escapeEntities
  ) where


import qualified Data.ByteString.Builder as BB
import           Data.Function ((.))
import           Data.Functor (Functor(..))
import           Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Lazy.Encoding as TLE

import           Hydrant.Data
import qualified Hydrant.Raw as Raw


toLazyText :: Html -> TL.Text
toLazyText =
  TLB.toLazyText . unHtml

toText :: Html -> Text
toText =
  TL.toStrict . toLazyText

toUtf8Builder :: Html -> BB.Builder
toUtf8Builder =
  TLE.encodeUtf8Builder . toLazyText

textNode :: Text -> Html
textNode =
  Html . Raw.textNode
{-# INLINE textNode #-}

textNodeUnescaped :: Text -> Html
textNodeUnescaped =
  Html . Raw.textNodeUnescaped
{-# INLINE textNodeUnescaped #-}

parentNode :: Tag -> [Attribute] -> Html -> Html
parentNode t attrs =
  Html . Raw.parentNode (unTag t) (fmap unAttribute attrs) . unHtml
{-# INLINE parentNode #-}

voidNode :: Tag -> [Attribute] -> Html
voidNode t =
  Html . Raw.voidNode (unTag t) . fmap unAttribute
{-# INLINE voidNode #-}
