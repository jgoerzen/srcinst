{- arch-tag: main builder
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

module Builder where
import MissingH.Logging.Logger
import MissingH.Debian
import MissingH.Debian.ControlParser
import MissingH.Either
import MissingH.Cmd
import System.Cmd
import System.Exit
import Dpkg
import Text.ParserCombinators.Parsec

buildOrInstall :: String -> IO ()

buildOrInstall packagename = 
    do infoM "" $ "Processing " ++ packagename
       installed <- getInstalledVer packagename
       avail <- getAvailableVer packagename
       infoM "" $ show (installed, avail)
       case (installed, avail) of
         (Nothing, Nothing) -> fail $ 
                           packagename ++ " is not available in source form"
         (Just _, Nothing) -> infoM "" $ packagename ++ " is already installed, and does not exist in source form"
         (Nothing, Just x) -> buildOrInstallRunner packagename x
         (Just inst, Just avail) -> 
             do c <- compareDebVersion inst avail
                if c /= LT
                   then infoM "" $ packagename ++ " " ++ inst ++
                                   " is already installed, and there is no newer version"
                   else buildOrInstallRunner packagename inst

-- Install from cache, or build
buildOrInstallRunner packagename version =
    do hc <- hascache packagename version
       if hc
          then installcache packagename version
          else do build packagename
                  installcache packagename version

hascache packagename version =
    do infoM "" $ "hascache: testing " ++ pkgVerToFilename packagename version
       rc <- rawSystem "bash" ["-c", "test -f " ++  pkgVerToFilename packagename version]
       case rc of
               ExitSuccess -> do debugM "" "hascache returning True"
                                 return True
               ExitFailure _ -> do debugM "" "hascache returnung False"
                                   return False
               
build packagename =
    do infoM "" $ "Beginning build of " ++ packagename
       procBuildDeps packagename
       safeSystem "apt-get" ["-b", "source", packagename]

installcache packagename version =
    do infoM "" $ "Scanning deps for " ++ packagename ++ " " ++ version
       procDebDeps packagename version
       infoM "" $ "Installing " ++ packagename ++ " " ++ version
       safeSystem "bash" ["-c", "dpkg -i " ++ (pkgVerToFilename packagename version)]

procDebDeps packagename version =
    do d <- getDebDeps packagename version
       procDeps d

procDeps deplist =
    let procThisDep packagedep =
            case forceEither $ parse depPart "(unknown)" packagedep of
             (pkg, Nothing) -> buildOrInstall pkg
             (pkg, Just (op, ver)) ->
                do installed <- getInstalledVer pkg
                   avail <- getAvailableVer pkg
                   case (installed, avail) of
                     (Nothing, Nothing) -> fail $ "No package for dependency " ++ packagedep ++ " is available"
                     (Just i, Nothing) -> 
                         do dv <- checkDebVersion i op ver
                            if dv
                               then return ()
                               else fail $ "No package in sufficient version for dependency " ++ packagedep ++ " is available"
                     (Nothing, Just x) -> 
                         do dv <- checkDebVersion x op ver
                            if dv
                               then buildOrInstallRunner pkg x
                               else fail $ "No package in sufficient source version for dependyncy " ++ packagedep
                     (Just x, Just y) -> 
                         do dv <- checkDebVersion x op ver
                            if dv
                               then return ()
                               else do dv2 <- checkDebVersion y op ver
                                       if dv2
                                          then buildOrInstallRunner pkg  y
                                          else fail $ "No package in sufficient source version for dep " ++ packagedep
    in
    mapM_ procThisDep deplist

procBuildDeps packagename =
    do bd <- getBuildDeps packagename
       procDeps bd
