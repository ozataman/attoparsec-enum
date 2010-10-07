{-# LANGUAGE DeriveDataTypeable #-}

module Data.Attoparsec.Enum
  ( iterParser ) where

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


iterParser :: (Monad m) => 
              Parser a
           -> Iteratee ByteString m a
iterParser p = continue $ f (\s -> parse p s)
  where
    f :: (Monad m) => 
         (ByteString -> Atto.Result a)
      -> Stream ByteString
      -> Iteratee ByteString m a
    f k EOF = endOfStream $ feed (k B.empty) B.empty
    f k (Chunks []) = continue $ f k
    f k (Chunks (x:xs)) = 
      case k x of
        Atto.Done s' r -> yield r $ if B.null s'
          then Chunks xs
          else Chunks (s':xs)
        Atto.Partial k' -> continue $ f k'
        Atto.Fail s' cxs e -> throwError $ ParserError $ "Parser encountered an error: " ++ e

    endOfStream :: (Monad m) => Atto.Result a -> Iteratee ByteString m a
    endOfStream (Atto.Done rest r) = yield r $ if B.null rest then EOF else Chunks [rest]
    endOfStream (Atto.Fail buf cxt e) = throwError $ ParserError $ 
      "Parser encountered error at EOF (stream end): " ++ e ++ "\n" ++
      "Context : " ++ show cxt ++ ". Remaining buffer: " ++ show buf
    endOfStream (Atto.Partial _) = throwError $ ParserError "Parser did not produce a value at EOF (stream end)"

