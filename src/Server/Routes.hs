{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Server.Routes where

import CodeExecutor
import Container.Algebra
import Container.Eff
import Control.Monad (msum)
import Control.Monad.IO.Class (liftIO)
import Happstack.Server
import Server.Context

root m = nullDir >> m

runCode :: Container -> Code -> IO (Either Error Result)
runCode c = withLxc . executeCode c

data ResponseData = ResponseData
  { status :: Int,
    message :: String,
    value :: Maybe String
  }

uffiRoutes ctx =
  [ dir "call" . path $ \mod -> path $ \fn -> root $ do
      method POST
      execId <- queryString $ look "exec_id"
      ok ("Call fn : " ++ mod ++ "." ++ fn),
    dir "variable" . path $ \var -> root $ do
      method GET
      execId <- queryString $ look "exec_id"
      value <- liftIO $ readVariable execId var ctx
      let resp =
            ResponseData
              { status = 200,
                message = "Variable not found",
                value = Just $ show value
              }
      ok ("Reading : " ++ var ++ "::" ++ show value),
    dir "variable" . path $ \var -> root $ do
      method POST
      execId <- queryString $ look "exec_id"
      liftIO $ putVariable execId var (MdString "fuck") ctx
      value <- liftIO $ readVariable execId var ctx
      ok ("Writing : " ++ var ++ "::" ++ show value)
  ]

mdRoutes ctx =
  [ dir "execute" . root $ do
      method POST
      result <- liftIO $ do
        container <- getCurrentContainer ctx
        nextContainer ctx
        putStrLn $ "Running on " ++ name container
        runCode container $ Bash "echo fuck off"
      ok $ "Executing markdown " ++ show result
  ]

routes ctx =
  msum $
    [root $ ok "Fuck"]
      ++ map (dir "uffi") (uffiRoutes ctx)
      ++ map (dir "md") (mdRoutes ctx)
      ++ [notFound "Not sure how you got here but fuck off please"]
