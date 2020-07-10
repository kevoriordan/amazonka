{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.EnvironmentStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticBeanstalk.Types.EnvironmentStatus (
  EnvironmentStatus (
    ..
    , ESLaunching
    , ESReady
    , ESTerminated
    , ESTerminating
    , ESUpdating
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data EnvironmentStatus = EnvironmentStatus' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern ESLaunching :: EnvironmentStatus
pattern ESLaunching = EnvironmentStatus' "Launching"

pattern ESReady :: EnvironmentStatus
pattern ESReady = EnvironmentStatus' "Ready"

pattern ESTerminated :: EnvironmentStatus
pattern ESTerminated = EnvironmentStatus' "Terminated"

pattern ESTerminating :: EnvironmentStatus
pattern ESTerminating = EnvironmentStatus' "Terminating"

pattern ESUpdating :: EnvironmentStatus
pattern ESUpdating = EnvironmentStatus' "Updating"

{-# COMPLETE
  ESLaunching,
  ESReady,
  ESTerminated,
  ESTerminating,
  ESUpdating,
  EnvironmentStatus' #-}

instance FromText EnvironmentStatus where
    parser = (EnvironmentStatus' . mk) <$> takeText

instance ToText EnvironmentStatus where
    toText (EnvironmentStatus' ci) = original ci

-- | Represents an enum of /known/ $EnvironmentStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum EnvironmentStatus where
    toEnum i = case i of
        0 -> ESLaunching
        1 -> ESReady
        2 -> ESTerminated
        3 -> ESTerminating
        4 -> ESUpdating
        _ -> (error . showText) $ "Unknown index for EnvironmentStatus: " <> toText i
    fromEnum x = case x of
        ESLaunching -> 0
        ESReady -> 1
        ESTerminated -> 2
        ESTerminating -> 3
        ESUpdating -> 4
        EnvironmentStatus' name -> (error . showText) $ "Unknown EnvironmentStatus: " <> original name

-- | Represents the bounds of /known/ $EnvironmentStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded EnvironmentStatus where
    minBound = ESLaunching
    maxBound = ESUpdating

instance Hashable     EnvironmentStatus
instance NFData       EnvironmentStatus
instance ToByteString EnvironmentStatus
instance ToQuery      EnvironmentStatus
instance ToHeader     EnvironmentStatus

instance FromXML EnvironmentStatus where
    parseXML = parseXMLText "EnvironmentStatus"
