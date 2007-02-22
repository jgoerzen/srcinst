{- arch-tag: dpkg querying
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

module Dpkg where
-- Interface to Dpkg/Apt

import System.Cmd.Utils
import System.Debian.ControlParser
import Data.Maybe.Utils
import System.Logging.Logger
import Data.String
import Text.Regex
import Utils

-- | Gets the installed version of a package, if any
getInstalledVer :: String -> IO (Maybe String)
getInstalledVer package = 
    do d <- readdata $ "dpkg -s " ++ package
       case d of 
              Nothing -> return Nothing
              Just x -> return $
                        if endswith "installed" $ forceMaybe $ lookup "Status" (parseControl x)
                           then lookup "Version" (parseControl x)
                           else Nothing

-- | Gets the available version of a package, if any
getAvailableVer :: String -> IO (Maybe String)
getAvailableVer package =
    do d <- readdata $ "apt-cache showsrc " ++ package
       case d of
              Nothing -> return Nothing
              Just x -> return $ lookup "Version" (parseControl x)

-- | Gets the build-deps of a package, if any
getBuildDeps :: String -> IO [String]
getBuildDeps package =
    do d <- readdata $ "apt-cache showsrc " ++ package
       case d of
          Nothing -> return []
          Just x -> let parsed = parseControl x
                        splitted y = map strip . split "," . filter (/= ' ') 
                                     $ y
                        find y = case lookup y parsed of
                                      Nothing -> []
                                      Just z -> splitted z
                        in
                        return $ find "Build-Depends" ++
                                 find "Build-Depends-Indep"

pkgVerToFilename :: String -> String -> String
pkgVerToFilename package version =
    let newvers = subRe (mkRegex "^[0-9]+:") version ""
        in
        package ++ "_" ++ newvers ++ "_*.deb"

-- | Gets the deps for a .deb, if any
getDebDeps :: String -> String -> IO [String]
getDebDeps package version =
    do d <- readdata $ "dpkg -f " ++ (pkgVerToFilename package version)
       case d of
              Nothing -> return []
              Just x -> let parsed = parseControl x
                            splitted y = map strip . split "," . 
                                         filter (/= ' ')$ y
                            find y = case lookup y parsed of
                                        Nothing -> []
                                        Just z -> splitted z
                            in
                            return $ find "Depends"
