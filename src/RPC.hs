module RPC
  ( Procedure
  , Parameters(..)
  , UserID(..)
  , ProcedureID(..)
  , ProcedureRegister(..)
  , execute
  ) where

import Network.Wai
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B

type Procedure = (Parameters -> IO Response)
data Parameters =
  Parameters UserID
             (M.Map B.ByteString B.ByteString)
  deriving (Show)
  
newtype UserID =
  UserID String
  deriving (Show)

data ProcedureID =
  ProcedureID [B.ByteString]
  deriving (Eq, Ord, Show)

data ProcedureRegister =
  ProcedureRegister (M.Map ProcedureID Procedure)

execute :: ProcedureRegister -> ProcedureID -> Parameters -> Maybe (IO Response)
execute (ProcedureRegister r) path params = do
  procedure <- M.lookup path r
  return $ procedure params
