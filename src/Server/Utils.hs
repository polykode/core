module Server.Utils where

import Happstack.Server

root m = nullDir >> m
