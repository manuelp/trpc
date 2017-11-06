module RPC
  ( Procedure
  , Parameters(..)
  , JsonString(..)
  , UserID(..)
  , ProcedureID(..)
  , ProcedureRegister(..)
  , execute
  ) where

import qualified Data.Map as M

type Procedure = (Parameters -> IO JsonString)
data Parameters =
  Parameters UserID
             (M.Map String String)
newtype JsonString =
  JsonString String
  deriving (Show)
newtype UserID =
  UserID String
  deriving (Show)

data ProcedureID =
  ProcedureID [String]
  deriving (Eq, Ord, Show)

data ProcedureRegister =
  ProcedureRegister (M.Map ProcedureID Procedure)

execute ::
     ProcedureRegister -> ProcedureID -> Parameters -> Maybe (IO JsonString)
execute (ProcedureRegister r) path params = do
  procedure <- M.lookup path r
  return $ procedure params
