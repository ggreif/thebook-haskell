{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.TheBook.Rule
-- Copyright   :  (c) 2014, Jakub Kozlowski
-- License     :  MIT
--
-- Maintainer  :  mail@jakub-kozlowski.com
--
-- Will eventually contain some sort of rule engine implementation,
-- for now let's try to come up with something.
-----------------------------------------------------------------------------
module Data.TheBook.Rule where

import           Control.Applicative       ((<$>), (<*>))
import           Control.Lens
import           Control.Lens.Getter       (view)
import           Control.Monad             (MonadPlus)
import           Control.Monad.Error.Class (MonadError, throwError)
import           Control.Monad.Reader      (MonadReader)
import           Control.Monad.Writer      (MonadWriter, tell)
import qualified Data.FIX.Parser           as FIX (messageP, nextP)
import           Data.TheBook.Book         (Book)
import qualified Data.TheBook.Book         as Book
import           Data.TheBook.Types        (OrderRejectReason, SessionID,
                                            WithDictionary, WithOrder,
                                            WithSession, dictL, orderL,
                                            sessionL)

data Command msg
  = SendMessage msg SessionID

-- | Sends a message to the current session.
replyWith :: (WithSession a, Functor m, Monad m, MonadReader a m, MonadWriter [Command msg] m) => msg -> m ()
replyWith msg = do
  ses <- view sessionL
  tell [SendMessage msg ses]

require :: MonadError e m => Bool -> e ->  m ()
require check e = if check
                  then return ()
                  else throwError e


-- | Type synomym for a rule that exists in order to validate
-- some aspect of an order. If this rule does not fire,
-- it means the validation was passed.
type Validation r e m = (Functor m, MonadReader r m, MonadError e m, MonadPlus m) => m ()

validatePrice :: (WithDictionary r, WithOrder r)
              => Validation r OrderRejectReason m
validatePrice = undefined

  --action <$> pure dictL <*> orderL
  --where action dict order = return ()


-- Ideas:
-- * Continue doing these small state/reader typeclasses, e.g. WithSession, WithBook
--   and make the functions in them actually lenses.
--   Then, depending on the type where those appear (MonadReader, MonadWriter),
--   use them appropriately.
-- * Reuse the Alternative typeclass for alternative rules, using <|>.
-- * Write the rules in exactly this style:
-- someValidation :: WithSession r
--                => Validation r SomeErrorType
-- where validation would be something like:
-- type Validation r e = (MonadReader r m, MonadError e m) => m ()
--
-- * The state With* classes would contain the At lens,
--   so it would be possible for a rule to fail, if the piece of state is not provided.
--   (Not yet sure if that's necessary)
--
-- * For rules that have some conditions, the conditions could be written in
--   applicative-like style (but that somehow encodes failure):
--
-- someRule :: (WithBook s, WithDictionary s)
--          => Rule s ()
-- someRule = bookL <*> dictL <*>
--            <)> (\book, dict) -> do
--                blablabla
--
-- Where <)> would be a reverse <$> operator, so it can be put at the end if possible
-- So the applicative-like part is gathering the conditions,
-- and part after <)> is the application of the rule that modifies state.
-- This would be nice, because *> could be used to just check a rule, but don't get it's result.
--
-- * From all this create just a massive struct that will unroll the monad:
-- data EngineState = EngineState {
--   _message :: Maybe Message
--   _event   :: Maybe Event
-- }
-- And provide all the With* instances.
-- The nice thing about this could be that this could be used with MonadReader
-- or MonadState, depending on what the rule is allowed to do, but have the same
-- underlying, unrolled representation.
--
-- Let's see if this is at all possible.

-- MonadError e m => MonadError e (ParsecT s u m)
-- MonadReader r m => MonadReader r (ParsecT s u m)
-- MonadState s m => MonadState s (ParsecT s' u m)
-- MonadTrans (ParsecT s u)
-- Monad (ParsecT s u m)
-- Functor (ParsecT s u m)
-- MonadPlus (ParsecT s u m)
-- Applicative (ParsecT s u m)
-- Alternative (ParsecT s u m)
-- MonadIO m => MonadIO (ParsecT s u m)
-- MonadCont m => MonadCont (ParsecT s u m)
