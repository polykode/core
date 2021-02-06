{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Server.Routes where

import CodeExecutor
import Container.Algebra
import Container.Eff
import Control.Monad (msum)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy.Char8 as ByteString
import qualified Data.Text as Text
import GHC.Generics
import Happstack.Server
import Server.Context
import qualified Server.Routes.Md as RMd
import qualified Server.Routes.Uffi as RUffi
import Server.Utils
import Utils

routes ctx =
  msum $
    [root $ ok "Fuck"]
      ++ map (dir "uffi") (RUffi.routes ctx)
      ++ map (dir "md") (RMd.routes ctx)
      ++ [notFound "Not sure how you got here but fuck off please"]
