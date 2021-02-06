{-# LANGUAGE FlexibleContexts #-}

module Server.Utils where

import Control.Monad (MonadPlus)
import Control.Monad.IO.Class (MonadIO)
import Happstack.Server

root m = nullDir >> m

decodePostBody :: (ServerMonad m, MonadPlus m, MonadIO m, FilterMonad Response m, WebMonad Response m) => m ()
decodePostBody = decodeBody (defaultBodyPolicy "/tmp/" 4096 4096 4096)
