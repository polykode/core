module Container.Pool where

import Container.Eff
import Control.Algebra
import Control.Monad (void)
import Utils

type ContainerPool = [Container]

-- Template container
template = Container "template-container"

createContainer :: Has LxcIOErr sig m => String -> m Container
createContainer name = do
  stat <- info $ Container name
  -- Only copy if it doesn't exist
  case stat of
    Left _ -> copy template name >>= start
    _ -> pure ()
  return $ Container name

cleanupContainer :: Has LxcIOErr sig m => Container -> m ()
cleanupContainer c = stop c >> delete c

toContainerName n = "container--" ++ show n

createContainerPool :: Has LxcIOErr sig m => Int -> [m Container]
createContainerPool count = map (createContainer . toContainerName) [1 .. count]

cleanContainerPool :: Has LxcIOErr sig m => ContainerPool -> m ()
cleanContainerPool = void . mapM cleanupContainer
