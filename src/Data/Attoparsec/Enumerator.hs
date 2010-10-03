{-# LANGUAGE DeriveDataTypeable #-}

module Data.Attoparsec.Enumerator
  ( parserToIteratee ) where

------------------------------------------------------------------------------
import           Control.Exception (Exception)
import qualified Data.Attoparsec as Atto
import           Data.Attoparsec hiding (many, Result(..))
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import           Data.Enumerator
import           Data.Data
------------------------------------------------------------------------------

data ParserError = ParserError String
  deriving (Show, Typeable)
instance Exception ParserError


parserToIteratee :: (Monad m) => 
                    Parser a
                 -> Iteratee ByteString m a
parserToIteratee p = continue $ f (\s -> parse p s)
  where
    f :: (Monad m) => 
         (ByteString -> Atto.Result a)
      -> Stream ByteString
      -> Iteratee ByteString m a
    f k EOF = endOfStream $ feed (k B.empty) B.empty
    f k (Chunks []) = continue $ f k
    f k (Chunks xs) = 
      let xs' = B.concat xs
      in case k xs' of
        Atto.Fail s' cxs e -> throwError $ ParserError $ "Parser encountered an error: " ++ e
        Atto.Done s' r -> yield r (Chunks [s'])
        Atto.Partial k' -> continue $ f k'

    endOfStream :: (Monad m) => Atto.Result a -> Iteratee ByteString m a
    endOfStream (Atto.Done rest r) = yield r (Chunks [rest])
    endOfStream (Atto.Fail _ _ e) = throwError $ ParserError $ "Parser encountered error at EOF (stream end) " ++ e 
    endOfStream (Atto.Partial _) = throwError $ ParserError "Parser did not produce a value at EOF (stream end)"

