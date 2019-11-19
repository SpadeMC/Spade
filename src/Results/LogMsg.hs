{-|
Module      : Errors
Description : Error-handling for spade
Copyright   : (c) Josh Findon and Edward Jones, 2019
License     : MIT
Maintainer  : Josh Findon and Edward Jones
Stability   : experimental
Portability : POSIX + Windows
Language    : Haskell2010

This module defines data structures to nicely handle log messages encountered during compilation
-}

module Results.LogMsg (LogMsg (..), Severity (..), printLogMsgs) where

import           Language.SpadeLexer (AlexPosn (..))
import           System.IO           (hPrint, stderr)

data Severity = Info | Warn | Error
    deriving (Ord, Eq)

instance Show Severity where
    show Info  = "Info"
    show Warn  = "Warning"
    show Error = "Error"

data LogMsg = LogMsg { severity :: Severity, locationInfo :: Maybe AlexPosn, message :: String }

instance Show LogMsg where
    show (LogMsg s (Just (AlexPn l n _)) m) = show s ++ ":" ++ show l ++ ':' : show n ++ ' ' : m
    show (LogMsg s Nothing m)               = show s ++ ":" ++ m

printLogMsgs :: [LogMsg] -> IO ()
printLogMsgs ms = sequence_ $ (hPrint stderr) <$> ms
