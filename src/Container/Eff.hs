{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Container.Eff where

import Control.Algebra
import Control.Effect.Throw
import Data.Kind (Type)
import GHC.IO.Exception

type Result = (ExitCode, String, String)

newtype Container = Container
  { name :: String
  }
  deriving (Show, Eq)

data Error = ContainerErr Int String | RunErr String
  deriving (Show, Eq)

data LxcEff (m :: Type -> Type) k where
  Start :: Container -> LxcEff m (Either Error ())
  Stop :: Container -> LxcEff m (Either Error ())
  Delete :: Container -> LxcEff m (Either Error ())
  Copy :: Container -> String -> LxcEff m (Either Error Container)
  Exec :: Container -> [String] -> LxcEff m Result
  Info :: Container -> LxcEff m (Either Error String)

data FileIOEff (m :: Type -> Type) k where
  CreateLocalFile :: String -> String -> FileIOEff m ()
  FilePush :: Container -> String -> String -> FileIOEff m ()

type LxcIOErr = LxcEff :+: FileIOEff :+: Throw Error

start :: Has LxcIOErr sig m => Container -> m ()
start c = send (Start c) >>= liftEither

stop :: Has LxcIOErr sig m => Container -> m ()
stop c = send (Stop c) >>= liftEither

delete :: Has LxcIOErr sig m => Container -> m ()
delete c = send (Delete c) >>= liftEither

info :: Has LxcIOErr sig m => Container -> m (Either Error String)
info = send . Info

copy :: Has LxcIOErr sig m => Container -> String -> m Container
copy c name = send (Copy c name) >>= liftEither

exec :: Has LxcIOErr sig m => Container -> [String] -> m Result
exec c = send . Exec c

filePush :: Has LxcIOErr sig m => Container -> String -> String -> m ()
filePush c f1 f2 = send $ FilePush c f1 f2

createFileInContainer :: Has LxcIOErr sig m => Container -> String -> String -> m ()
createFileInContainer c filePath contents = do
  send $ CreateLocalFile tmpPath contents
  filePush c tmpPath filePath
  where
    tmpPath = "/tmp/" ++ name c ++ "-" ++ toFlatFileName filePath
    toFlatFileName f = "file" ++ map replacer f
      where
        replacer '/' = '-'
        replacer c = c

--
--
