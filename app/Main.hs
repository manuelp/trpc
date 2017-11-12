{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Map as M
import Data.Maybe
import RPC
import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)
import Data.ByteString.Char8 (unpack)
import qualified Data.Text as T
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
  putStrLn . show . queryString $ request
  putStrLn . show $ path
  putStrLn . show $ params
  fn res >>= respond
  where
    path = ProcedureID . map (T.unpack) . pathInfo $ request
    params = getParameters (UserID "test") . queryString $ request
    res :: Maybe (IO JsonString)
    res = execute register path params

buildResponse :: Status -> JsonString -> Response
buildResponse s j =
  responseLBS s [("Content-Type", "application/json")] (encode . show $ j)

fn :: Maybe (IO JsonString) -> IO Response
fn (Just x) = do
  res <- x
  return $ buildResponse status200 res
fn Nothing = return . buildResponse status404 . JsonString $ "Unknown procedure"

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

register :: ProcedureRegister
register =
  ProcedureRegister $
  M.fromList [(ProcedureID ["demo", "concat"], concatProcedure)]

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
