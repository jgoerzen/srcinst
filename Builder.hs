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
import Utils
import Text.ParserCombinators.Parsec
import MissingH.Str
import Control.Monad

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
    -- Returns True if a package could be installed, or False otherwise.
    let procPkg :: (String, Maybe (String, String)) -> IO Bool
        procPkg (pkg, Nothing) = do buildOrInstall pkg
                                    return True
        procPkg (pkg, Just (op, ver)) =
            do installed <- getInstalledVer pkg
               avail <- getAvailableVer pkg
               case (installed, avail) of
                  (Nothing, Nothing) -> do infoM "" $ "No package for dependency " ++ pkg ++ " is available"
                                           return False
                  (Just i, Nothing) -> 
                      do dv <- checkDebVersion i op ver
                         if dv
                            then return True
                            else do infoM "" $ "No package in sufficient version for dependency " ++ pkg ++ " is available"
                                    return False
                  (Nothing, Just x) -> 
                      do dv <- checkDebVersion x op ver
                         if dv
                            then do buildOrInstallRunner pkg x
                                    return True
                            else do infoM "" $ "No package in sufficient source version for dependyncy " ++ pkg
                                    return False
                  (Just x, Just y) -> 
                      do dv <- checkDebVersion x op ver
                         if dv
                            then return True
                            else do dv2 <- checkDebVersion y op ver
                                    if dv2
                                       then do buildOrInstallRunner pkg  y
                                               return True
                                       else do infoM "" $ "No package in sufficient source version for dep " ++ pkg
                                               return False

        procSpecificDep True _ = return True -- dep already satisfied
        procSpecificDep False (package, version, _) = 
            procPkg (package, version)
        splitdeps = map strip . split "|"
        procThisDep dep =
            do myarch <- getArch
               debugM "" $ "procThisDep: my arch is " ++ myarch
               let deplist = splitdeps dep
               let parsedeplist = map 
                                   (\x -> forceEither .
                                    parse depPart x $ x) deplist
               let filteredlist = 
                       filter (\(_, _, archlist) -> archlist == [] || elem myarch archlist) parsedeplist
               case filteredlist of
                [] -> return ()
                _ -> do r <- foldM procSpecificDep False filteredlist
                        if r
                           then return ()
                           else fail "Failed to meet all deps"
    in
    mapM_ procThisDep deplist

procBuildDeps packagename =
    do bd <- getBuildDeps packagename
       procDeps bd
