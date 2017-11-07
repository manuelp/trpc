{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Map as M
import Data.Maybe
import RPC
import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)
import Data.ByteString.Char8 (unpack)
import Data.Aeson (encode)

main :: IO ()
main = do
    putStrLn $ "http://localhost:8080/"
    run 8080 app

-- https://www.stackage.org/haddock/lts-9.12/wai-3.2.1.1/Network-Wai.html#t:Application
--
-- type Application
--    = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
app :: Application
app request respond = do
  putStrLn "I've done some IO here"
  putStrLn . show $ params
  res <- concatProcedure params
  respond $
    responseLBS
      status200
      [("Content-Type", "application/json")]
      (encode . show $ res)
  where
    user = UserID "test"
    params = getParameters (UserID "test") . queryString $ request

-- https://www.stackage.org/haddock/lts-9.12/http-types-0.9.1/Network-HTTP-Types-URI.html#t:Query
--
-- type Query = [QueryItem]
-- type QueryItem = (ByteString, Maybe ByteString) 
getParameters :: UserID -> Query -> Parameters
getParameters u q =
  let f (x, Just y) = Just (unpack x, unpack y)
      f _ = Nothing
      params = concat $ map (maybeToList . f) q
  in Parameters u $ M.fromList params

--
-- ----------------------------------------
--

concatProcedure :: Procedure
concatProcedure =
  \(Parameters _ m) ->
    case (getParams m) of
      Just (x, y) -> return $ JsonString (x ++ y)
      otherwise -> fail "Missing parameters!"
  where
    getParams m = do
      x <- M.lookup "x" m
      y <- M.lookup "y" m
      return $ (x, y)

register :: ProcedureRegister
register =
  ProcedureRegister $
  M.fromList [(ProcedureID ["demo", "concat"], concatProcedure)]

-- main :: IO ()
-- main = do
--   case res of
--     Just action -> fmap show action >>= putStrLn
--     Nothing -> putStrLn "Procedure not found!"
--   where
--     res = execute register (ProcedureID ["demo", "concat"]) dummyInput
