module App.Main where

import App.API
import App.WsApp
import Network.Wai.Middleware.Cors
  ( CorsResourcePolicy (corsMethods, corsOrigins, corsRequestHeaders),
    cors,
    simpleCorsResourcePolicy,
  )
import Network.Wai.Handler.Warp (run)
import Prelude


-- Run the Warp server on port 8080
main :: IO ()
main = do
  putStrLn "Server running on port 8080"
  run 8080 $ cors (const $ Just corsPolicy) app
  where
    corsPolicy :: CorsResourcePolicy
    corsPolicy =
      simpleCorsResourcePolicy
        { corsOrigins = Nothing, -- allows all origins
          corsRequestHeaders = ["Content-Type"], -- allow only these request headers
          corsMethods = ["GET", "POST"] -- allow only these methods
        }