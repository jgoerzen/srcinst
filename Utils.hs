{- arch-tag: srcinst utils
Copyright (C) 2004 John Goerzen <jgoerzen@complete.org>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

module Utils where
import MissingH.Cmd
import MissingH.Debian.ControlParser
import MissingH.Either
import MissingH.Logging.Logger
import MissingH.List
import MissingH.Str
import System.IO.Error
import System.IO
import Text.ParserCombinators.Parsec
import MissingH.Maybe

readdata :: String -> IO (Maybe String)
readdata command =
    let readcmd h = do c <- hGetContents h
                       return $! seqList c
        in
        catch (pOpen ReadFromPipe "bash" ["-c", command ++ " 2>/dev/null"] readcmd >>= return . Just)
              (\e -> do debugM "Utils" ("readdata " ++ command ++
                                                        ": " ++ show e)
                        return Nothing)

parseControl :: String -> [(String, String)]
parseControl inp = map (\(f,s) -> (f, strip s)) $ 
                     forceEither $ parse control "(unknown)" inp


getArch :: IO String
getArch = do c <- readdata "dpkg --print-architecture"
             return $ strip $ forceMaybe c
