module JSTP.Colors where

green :: String -> String
green = applyColor "\x1b[32m"

blue :: String -> String
blue = applyColor "\x1b[34m"

red :: String -> String
red = applyColor "\x1b[31m"

applyColor :: String -> String -> String
applyColor begin str = concat [begin, str, "\x1b[0m"]
