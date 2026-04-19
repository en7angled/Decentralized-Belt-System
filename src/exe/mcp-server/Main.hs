module Main where

import MCPServer.App (withAppCtx)
import MCPServer.Server (runMCPServer)

main :: IO ()
main = withAppCtx runMCPServer
