module Container.Pool where

import Container.Eff
import Control.Algebra
import Control.Monad (void)
import Utils

-- Template container
template = Container "template-container"

createContainer :: Has LxcIOErr sig m => String -> m Container
createContainer name = Container name <$ (copy template name >>= start)

cleanupContainer :: Has LxcIOErr sig m => Container -> m ()
cleanupContainer c = stop c >> delete c

toContainerName n = "container--" ++ show n

createContainerPool :: Has LxcIOErr sig m => Int -> m [Container]
createContainerPool count = concatM . map (createContainer . toContainerName) $ [1 .. count - 1]

cleanContainerPool :: Has LxcIOErr sig m => [Container] -> m ()
cleanContainerPool = void . concatM . map cleanupContainer
