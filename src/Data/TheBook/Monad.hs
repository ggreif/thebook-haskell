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
) where

import           Control.Applicative  (Applicative, pure, (<*>))
import           Control.Monad        (Monad, MonadPlus, ap, mplus, mzero,
                                       (=<<))
import           Control.Monad.Error  (Error, MonadError, catchError,
                                       throwError)
import           Control.Monad.Reader (MonadReader)
import           Control.Monad.State  (MonadState)
import           Control.Monad.Writer (MonadWriter, tell)
import           Data.Functor         (Functor)
import           Data.Monoid          (Monoid, mempty, (<>))
import qualified Data.TheBook.Types   as Types

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
-- This type is an instance of the following classes:
-- * 'Monad', where 'fail' throws an exception,
--   'return' constitues a 'Match' and '(>>=)'
--   does not evaluate the right hand side if
--   the left hand side terminates with 'NoMatch'
--   or 'Exception'.
newtype Rule s w e a = Rule { runRule :: s -> Result s w e a }

instance (Monoid w, Error e) => Monad (Rule s w e) where
  return = returnR
  (>>=)  = bindR
  fail   = failR

instance Functor (Rule s w e) where
  fmap f m = Rule $ \s -> case runRule m s of
    Match     s' w   a -> Match s' w (f a)
    NoMatch            -> NoMatch
    Exception      e   -> Exception e

instance (Monoid w, Error e) => Applicative (Rule s w e) where
  pure = returnR
  (<*>) = ap

instance (Monoid w, Error e) => MonadPlus (Rule s w e) where
  mzero = mzeroR
  mplus = mplusR

instance (Monoid w, Error e) => MonadError e (Rule s w e) where
  throwError e = Rule $ \_ -> Exception e
  catchError = catchErrorR

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

{-# INLINE mzeroR #-}
mzeroR :: Rule s w e a
mzeroR = Rule $ \_ -> NoMatch

{-# INLINE mplusR #-}
mplusR :: (Monoid w, Error e)
       => Rule s w e a
       -> Rule s w e a
       -> Rule s w e a
mplusR l r = bindR l (const r)

{-# INLINE catchErrorR #-}
catchErrorR :: (Monoid w, Error e)
            => Rule s w e a
            -> (e -> Rule s w e a)
            -> Rule s w e a
catchErrorR m f = Rule $ \s -> case runRule m s of
  match@(Match _ _ _) -> match
  NoMatch -> NoMatch
  Exception e -> runRule (f e) s

