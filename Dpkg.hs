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
import MissingH.Cmd
import MissingH.Debian.ControlParser
import MissingH.Maybe
import MissingH.Logging.Logger
import Utils

-- | Gets the installed version of a package, if any
getInstalledVer :: String -> IO (Maybe String)
getInstalledVer package = 
    do d <- readdata "dpkg" ["-s", package]
       --debugM "Dpkg" ("Got: " ++ show d)
       case d of 
              Nothing -> return Nothing
              Just x -> return $ Just $ forceMaybe $ lookup "Version" (parseControl x)
