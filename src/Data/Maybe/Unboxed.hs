{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE DeriveDataTypeable #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Data.Maybe.Unboxed (
    Maybe(..)
 ,  pattern Nothing
 ,  pattern Just
 ,  isJust
 ,  isNothing
 ,  fromMaybe
 ,  fromJust
 ,  maybeToList
 ,  toMaybeUnboxed
 ,  fromMaybeUnboxed
 ,  headMaybe
 ,  initMaybe
 ,  lastMaybe
 ,  tailMaybe
 ,  maybe
) where

import Prelude hiding (Maybe(..), maybe, null)
import qualified Prelude as P (Maybe(..))

import GHC.Stack.Types ( HasCallStack )

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.List as L
import Data.Typeable ( Typeable )

data Maybe a = Maybe (# (# #) | a #)
              deriving (Typeable)

pattern Nothing :: Maybe a
pattern Nothing = Maybe (# (# #) | #)

pattern Just :: a -> Maybe a
pattern Just a  = Maybe (# | a #)


isNothing :: Maybe a -> Bool
isNothing m = case m of
                Maybe (# (# #) | #) -> True
                _ -> False
{-# INLINE isNothing #-}


isJust :: Maybe a -> Bool
isJust m = case m of
                Maybe (# (# #) | #) -> False
                _ -> True
{-# INLINE isJust #-}

fromMaybe  :: a -> Maybe a -> a
fromMaybe _ (Just v) = v
fromMaybe d _ = d
{-# INLINE fromMaybe #-}

fromJust :: HasCallStack => Maybe a -> a
fromJust (Just x) = x
fromJust _ = error "Maybe.Unboxed.fromJust: Nothing"
{-# INLINE fromJust #-}

maybeToList :: Maybe a -> [a]
maybeToList  (Just x)  = [x]
maybeToList  _ = []
{-# INLINE maybeToList #-}

maybe :: b -> (a -> b) -> Maybe a -> b
maybe _ f (Just x) = f x
maybe n _ _ = n
{-# inline maybe #-}


instance Functor Maybe where
  fmap f (Just a) = Just (f a)
  fmap _ _        = Nothing

instance Show a => Show (Maybe a) where
    show (Just a) = "Just " <> show a
    show _        = "Nothing"

instance Eq a => Eq (Maybe a) where
    Just x  == Just y  = x == y
    Nothing == Nothing = True
    _       ==  _      = False

instance Ord a => Ord (Maybe a) where
  compare (Just a) (Just b) = compare a b
  compare _        (Just _) = LT
  compare (Just _) _        = GT
  compare _        _        = EQ


toMaybeUnboxed :: P.Maybe a -> Maybe a
toMaybeUnboxed (P.Just x) = Just x
toMaybeUnboxed _          = Nothing
{-# INLINE toMaybeUnboxed #-}

fromMaybeUnboxed :: Maybe a -> P.Maybe a
fromMaybeUnboxed (Just x) = P.Just x
fromMaybeUnboxed _        = P.Nothing
{-# INLINE fromMaybeUnboxed #-}


instance A.ToJSON a => A.ToJSON (Maybe a) where
    toJSON (Just x) = A.toJSON x
    toJSON _        = A.Null
    {-# INLINE toJSON #-}

instance A.FromJSON1 Maybe where
    liftParseJSON _ _ _ A.Null = pure Nothing
    liftParseJSON _ p _ a      = Just <$> p a
    {-# INLINE liftParseJSON #-}

instance (A.FromJSON a) => A.FromJSON (Maybe a) where
    parseJSON = parseJSON1
    {-# INLINE parseJSON #-}

parseJSON1 :: (A.FromJSON1 f, A.FromJSON a) => A.Value -> A.Parser (f a)
parseJSON1 = A.liftParseJSON A.omittedField A.parseJSON A.parseJSONList
{-# INLINE parseJSON1 #-}

totalList :: Foldable t => (t a -> b) -> t a -> Maybe b
totalList f xs
  | L.null xs = Nothing
  | otherwise = Just $ f xs
{-# INLINABLE  totalList #-}

headMaybe :: [a] -> Maybe a
headMaybe = totalList L.head
{-# INLINE headMaybe #-}

lastMaybe :: [a] -> Maybe a
lastMaybe = totalList L.last
{-# INLINE lastMaybe #-}

initMaybe :: [a] -> Maybe [a]
initMaybe = totalList L.init
{-# INLINE initMaybe #-}

tailMaybe :: [a] -> Maybe [a]
tailMaybe = totalList L.tail
{-# INLINE tailMaybe #-}
