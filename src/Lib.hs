module Lib
  ( Procedure
  , Parameters(..)
  , UserID(..)
  ) where

import Network.Wai
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B

type Procedure = Parameters -> IO Response

data Parameters =
  Parameters UserID
             (M.Map B.ByteString B.ByteString)
  deriving (Show)
  
newtype UserID =
  UserID String
  deriving (Show)

