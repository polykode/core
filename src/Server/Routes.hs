{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module Server.Routes where

import Config
import Control.Monad (msum)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Aeson as Json
import GHC.Generics
import Happstack.Server
import Server.JsonResponse
import qualified Server.Routes.Md as RMd
import qualified Server.Routes.Uffi as RUffi
import Server.Utils
import System.Process

rootHandler :: (FilterMonad Response m, MonadIO m) => m String
rootHandler = do
  (_, uptime, _) <- liftIO $ readProcessWithExitCode "uptime" [] ""
  json ok $
    JsonResponse Success "Fuck off" (Just . toJsonString $ uptime)

versionAction :: (FilterMonad Response m) => m String
versionAction =
  json ok $
    JsonResponse Success "Version" (Just . toJsonString $ version)

routes ctx =
  msum $
    [root rootHandler, dir "version" versionAction]
      ++ map (dir "uffi") (RUffi.routes ctx)
      ++ map (dir "md") (RMd.routes ctx)
      ++ [json notFound $ JsonResponse Success "Not sure how you got here but fuck off please" Nothing]
