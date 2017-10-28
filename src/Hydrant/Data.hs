{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Hydrant.Data (
    Html (..)
  , Tag (..)
  , Attribute (..)
  , unAttribute
  , AttributeKey (..)
  , AttributeValue (..)
  ) where


import           Data.Functor (Functor (..))
import           Data.Monoid (Monoid(..))
import           Data.Text (Text)
import           Data.Text.Lazy.Builder (Builder)

import           Prelude (Eq(..), Ord(..), Show(..))


newtype Html = Html {
    unHtml :: Builder
  } deriving (Eq, Ord, Show)

instance Monoid Html where
  mempty = Html mempty
  {-# INLINE mempty #-}

  mappend (Html a) (Html b) = Html (mappend a b)
  {-# INLINE mappend #-}

  mconcat as = Html (mconcat (fmap unHtml as))
  {-# INLINE mconcat #-}

newtype Tag = Tag {
    unTag :: Text
  } deriving (Eq, Ord, Show)

data Attribute
  = Attribute AttributeKey AttributeValue
  deriving (Eq, Ord, Show)

unAttribute :: Attribute -> (Text, Text)
unAttribute a =
  case a of
    Attribute (AttributeKey k) (AttributeValue v) ->
      (k, v)
{-# INLINE unAttribute #-}

newtype AttributeKey = AttributeKey {
    unAttributeKey :: Text
  } deriving (Eq, Ord, Show)

newtype AttributeValue = AttributeValue {
    unAttributeValue :: Text
  } deriving (Eq, Ord, Show)
