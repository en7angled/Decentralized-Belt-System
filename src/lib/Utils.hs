module Utils where

greenColorString :: String -> String
greenColorString s =
  "\n"
    ++ "\ESC[1;32m"
    ++ s
    ++ "\ESC[0m"
    ++ "\n"

orangeColorString :: String -> String
orangeColorString s =
  "\n"
    ++ "\ESC[1;33m"
    ++ s
    ++ "\ESC[0m"
    ++ "\n"


yellowColorString :: String -> String
yellowColorString s =
  "\n"
    ++ "\ESC[1;93m"
    ++ s
    ++ "\ESC[0m"
    ++ "\n"

blueColorString :: String -> String
blueColorString s =
  "\n"
    ++ "\ESC[1;94m"
    ++ s
    ++ "\ESC[0m"
    ++ "\n"

purpleColorString :: String -> String
purpleColorString s =
  "\n"
    ++ "\ESC[1;95m"
    ++ s
    ++ "\ESC[0m"
    ++ "\n"
