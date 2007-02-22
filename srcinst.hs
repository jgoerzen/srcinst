{- arch-tag: srcinst main file
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

module Main where
import System.Environment
import System.Directory
import System.Exit
import Builder
import Dpkg
import System.Logging.Logger

syntaxError :: IO ()
syntaxError =
    do putStrLn "Syntax: srcinst install package1 [package2 ... packagen]"
       exitFailure

main = do args <- getArgs
          setCurrentDirectory "/var/cache/srcinst"
          updateGlobalLogger rootLoggerName (setLevel DEBUG)
          updateGlobalLogger "System.Cmd.Utils.pOpen3" (setLevel CRITICAL)
          case args of
               "install":xs -> mapM_ buildOrInstall xs
               _ -> syntaxError
