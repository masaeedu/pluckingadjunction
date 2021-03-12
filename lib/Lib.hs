{-# LANGUAGE StandaloneKindSignatures, ImpredicativeTypes #-}
module Lib where

import Data.Kind
import Data.Functor.Compose
import Control.Arrow ((&&&))
import Data.Bifunctor

type (:×:) :: (k -> *) -> (k -> *) -> k -> *
data (f :×: g) x = f x :×: g x

type (:∘:) :: (b -> *) -> (a -> b) -> a -> *
type (:∘:) = Compose

unpack :: (f :×: g) x -> (f x, g x)
unpack (fx :×: gx) = (fx, gx)

repack :: (f x, g x) -> (f :×: g) x
repack (fx, gx) = fx :×: gx

type Monad' :: (* -> *) -> *
data Monad' m = Monad' { pure' :: forall a. a -> m a, bind' :: forall a b. m a -> (a -> m b) -> m b }

type MonadReader :: * -> (* -> *) -> *
newtype MonadReader env m = MonadReader { get :: m env }

type ReaderT' :: * -> ((* -> *) -> *) -> ((* -> *) -> *)
newtype ReaderT' env h m = ReaderT' { runReaderT' :: h ((->) env :∘: m) }

type (~>) :: (* -> *) -> (* -> *) -> *
type m1 ~> m2 = forall x. m1 x -> m2 x

type (~~>) :: ((* -> *) -> *) -> ((* -> *) -> *) -> *
type h1 ~~> h2 = forall m. Monad' m -> h1 m -> h2 m

type SomeFunctor :: ((* -> *) -> *) -> Constraint
class SomeFunctor h
  where
  somemap :: (a ~> b) -> h a -> h b

unit :: SomeFunctor h => h ~~> ReaderT' env (h :×: MonadReader env)
unit (Monad' pure' _) = ReaderT' . repack . (somemap (Compose . const) &&& MonadReader . Compose . const pure')

counit :: SomeFunctor h => ReaderT' env h :×: MonadReader env ~~> h
counit (Monad' _ bind') = (\(f, get) -> somemap (bind' get . getCompose) f) . bimap runReaderT' get . unpack

message :: String
message = "Ready? Get set... GO!"
