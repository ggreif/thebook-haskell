{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.TheBook.Monad
-- Copyright   :  (c) 2014, Jakub Kozlowski
-- License     :  MIT
--
-- Maintainer  :  mail@jakub-kozlowski.com
--
-- Unrolled concrete monad that can be used by the rules.
-----------------------------------------------------------------------------
module Data.TheBook.Monad (
    Result(..)
  , Rule, runRule
) where

import           Control.Applicative        (Alternative, Applicative, empty,
                                             pure, (<*>), (<|>))
import           Control.Monad              (MonadPlus, ap, mplus, mzero)
import           Control.Monad.Error        (Error, MonadError, catchError,
                                             throwError)
import           Control.Monad.Reader.Class (MonadReader, ask, local)
import           Control.Monad.State.Class  (MonadState, get, put, state)
import           Control.Monad.Writer.Class (MonadWriter, listen, pass)
import           Data.Monoid                (Monoid, mempty, (<>))

-- | The result of trying to match a rule.
-- This is parameterised over:
-- * @s@ type of the read/write state
-- * @w@ type of the log
-- * @e@ type of exceptions
-- * @a@ type of the result
data Result   s w e a
  = Match     s w   a
    -- ^ The rule fired successfully.
  | NoMatch
    -- ^ The rule did not fire.
  | Exception     e
    -- ^ The rule threw an exception.

-- | Specific monad that can be used to run rules.
--
-- This type is an instance of the following classes:
--
-- * 'Monad', where 'fail' throws an exception,
--   'return' constitues a 'Match' and '(>>=)'
--   does not evaluate the right hand side if
--   the left hand side terminates with 'NoMatch'
--   or 'Exception'.
--
-- *
newtype Rule s w e a = Rule { runRule :: s -> Result s w e a }

instance (Monoid w, Error e) => Monad (Rule s w e) where
  return = returnR
  (>>=)  = bindR
  fail   = failR

instance Functor (Rule s w e) where
  fmap = fmapR

instance (Monoid w, Error e) => Applicative (Rule s w e) where
  pure = returnR
  (<*>) = ap

instance (Monoid w, Error e) => Alternative (Rule s w e) where
  empty = mzeroR
  (<|>) = orR

instance (Monoid w, Error e) => MonadPlus (Rule s w e) where
  mzero = mzeroR
  mplus = mplusR

instance (Monoid w, Error e) => MonadError e (Rule s w e) where
  throwError = throwErrorR
  catchError = catchErrorR

instance (Monoid w, Error e) => MonadState s (Rule s w e) where
  get   = getR
  put   = putR
  state = stateR

instance (Monoid w, Error e) => MonadReader s (Rule s w e) where
  ask   = askR
  local = localR

instance (Monoid w, Error e) => MonadWriter w (Rule s w e) where
  listen = listenR
  pass   = passR

{-# INLINE returnR #-}
returnR :: Monoid w => a -> Rule s w e a
returnR x = Rule $ \s -> Match s mempty x

{-# INLINE bindR #-}
bindR :: Monoid w
      => Rule s w e a
      -> (a -> Rule s w e b)
      -> Rule s w e b
bindR m f = Rule $ \s -> case runRule m s of
  Match     s' w   a -> case runRule (f a) s' of
    Match s'' w' b -> Match s'' (w <> w') b
    NoMatch        -> NoMatch
    Exception e    -> Exception e
  NoMatch            -> NoMatch
  Exception      e   -> Exception e

{-# INLINE failR #-}
failR :: String -> Rule s w e a
failR = error

{-# INLINE fmapR #-}
fmapR :: (a -> b) -> Rule s w e a -> Rule s w e b
fmapR f m = Rule $ \s -> case runRule m s of
    Match     s' w   a -> Match s' w (f a)
    NoMatch            -> NoMatch
    Exception      e   -> Exception e

{-# INLINE mzeroR #-}
mzeroR :: Rule s w e a
mzeroR = Rule $ const NoMatch

{-# INLINE mplusR #-}
mplusR :: (Monoid w, Error e)
       => Rule s w e a
       -> Rule s w e a
       -> Rule s w e a
mplusR l r = bindR l (const r)

{-# INLINE throwErrorR #-}
throwErrorR :: (Monoid w, Error e)
            => e
            -> Rule s w e a
throwErrorR e = Rule $ const (Exception e)

{-# INLINE catchErrorR #-}
catchErrorR :: (Monoid w, Error e)
            => Rule s w e a
            -> (e -> Rule s w e a)
            -> Rule s w e a
catchErrorR m f = Rule $ \s -> case runRule m s of
  match@(Match _ _ _) -> match
  NoMatch             -> NoMatch
  Exception e         -> runRule (f e) s

{-# INLINE getR #-}
getR :: (Monoid w) => Rule s w e s
getR = Rule $ \s -> Match s mempty s

{-# INLINE putR #-}
putR :: (Monoid w) => s -> Rule s w e ()
putR s = Rule $ \_ -> Match s mempty ()

{-# INLINE stateR #-}
stateR :: (Monoid w) => (s -> (a, s)) -> Rule s w e a
stateR f = Rule $ \s -> case f s of (a, s') -> Match s' mempty a

{-# INLINE askR #-}
askR :: (Monoid w) => Rule s w e s
askR = Rule $ \s -> Match s mempty s

{-# INLINE localR #-}
localR :: (Monoid w, Error e) => (s -> s) -> Rule s w e a -> Rule s w e a
localR f m = Rule $ \s -> runRule m (f s)

{-# INLINE listenR #-}
listenR :: (Monoid w, Error e) => Rule s w e a -> Rule s w e (a, w)
listenR = undefined

{-# INLINE passR #-}
passR :: (Monoid w, Error e) => Rule s w e (a, w -> w) -> Rule s w e a
passR m = Rule $ \s -> case runRule m s of
  Match s' w (a, fw)  -> Match s' (fw w) a
  NoMatch             -> NoMatch
  Exception e         -> Exception e

{-# INLINE orR #-}
orR :: (Monoid w, Error e) => Rule s w e a -> Rule s w e a -> Rule s w e a
orR left right = Rule $ \s -> case runRule left s of
  m@(Match      _ _   _)  -> m
  NoMatch                 -> runRule right s
  ex@(Exception     _  )  -> ex
