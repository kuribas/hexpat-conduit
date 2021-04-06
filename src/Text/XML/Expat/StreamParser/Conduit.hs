{-# LANGUAGE RankNTypes #-}
module Text.XML.Expat.StreamParser.Conduit where
import Text.XML.Expat.StreamParser
import Text.XML.Expat.SAX
import Data.List.Class
import Control.Monad.ListT (ListT(..))
import Data.Conduit
import Data.Text
import Data.ByteString
import Data.Functor.Identity

-- | An EventParser in the ConduitT monad.
type ConduitEventParser e o m a =
  (forall i.EventParser (ListT (ConduitT i o m)) e (ConduitT i o m) a)

-- | Convert an EventParser in the ConduitT monad to a ConduitT that
-- takes bytestrings and produces the result of the parser.  You can
-- use `lift . yield` to stream values as a side effect in the
-- ConduitT monad, or `lift effect` to run any effect in the ConduitT
-- monad.
streamParserToConduit :: Monad m
                      => ParseOptions Text Text
                      -> ConduitEventParser e o m a
                      -> ConduitT ByteString o m (Either (EventParseError e,
                                                          Maybe
                                                          XMLParseLocation)
                                                  a)
streamParserToConduit opts parser =
  runEventParser parser $ parseLocationsG opts bsStream
  where
    bsStream = ListT $ do
      next <- await
      case next of
        Nothing -> pure Nil
        Just el -> pure $ Cons el bsStream

