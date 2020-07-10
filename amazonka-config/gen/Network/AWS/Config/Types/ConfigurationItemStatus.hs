{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ConfigurationItemStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Config.Types.ConfigurationItemStatus (
  ConfigurationItemStatus (
    ..
    , OK
    , ResourceDeleted
    , ResourceDeletedNotRecorded
    , ResourceDiscovered
    , ResourceNotRecorded
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ConfigurationItemStatus = ConfigurationItemStatus' (CI
                                                           Text)
                                 deriving (Eq, Ord, Read, Show, Data, Typeable,
                                           Generic)

pattern OK :: ConfigurationItemStatus
pattern OK = ConfigurationItemStatus' "OK"

pattern ResourceDeleted :: ConfigurationItemStatus
pattern ResourceDeleted = ConfigurationItemStatus' "ResourceDeleted"

pattern ResourceDeletedNotRecorded :: ConfigurationItemStatus
pattern ResourceDeletedNotRecorded = ConfigurationItemStatus' "ResourceDeletedNotRecorded"

pattern ResourceDiscovered :: ConfigurationItemStatus
pattern ResourceDiscovered = ConfigurationItemStatus' "ResourceDiscovered"

pattern ResourceNotRecorded :: ConfigurationItemStatus
pattern ResourceNotRecorded = ConfigurationItemStatus' "ResourceNotRecorded"

{-# COMPLETE
  OK,
  ResourceDeleted,
  ResourceDeletedNotRecorded,
  ResourceDiscovered,
  ResourceNotRecorded,
  ConfigurationItemStatus' #-}

instance FromText ConfigurationItemStatus where
    parser = (ConfigurationItemStatus' . mk) <$> takeText

instance ToText ConfigurationItemStatus where
    toText (ConfigurationItemStatus' ci) = original ci

-- | Represents an enum of /known/ $ConfigurationItemStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ConfigurationItemStatus where
    toEnum i = case i of
        0 -> OK
        1 -> ResourceDeleted
        2 -> ResourceDeletedNotRecorded
        3 -> ResourceDiscovered
        4 -> ResourceNotRecorded
        _ -> (error . showText) $ "Unknown index for ConfigurationItemStatus: " <> toText i
    fromEnum x = case x of
        OK -> 0
        ResourceDeleted -> 1
        ResourceDeletedNotRecorded -> 2
        ResourceDiscovered -> 3
        ResourceNotRecorded -> 4
        ConfigurationItemStatus' name -> (error . showText) $ "Unknown ConfigurationItemStatus: " <> original name

-- | Represents the bounds of /known/ $ConfigurationItemStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ConfigurationItemStatus where
    minBound = OK
    maxBound = ResourceNotRecorded

instance Hashable     ConfigurationItemStatus
instance NFData       ConfigurationItemStatus
instance ToByteString ConfigurationItemStatus
instance ToQuery      ConfigurationItemStatus
instance ToHeader     ConfigurationItemStatus

instance FromJSON ConfigurationItemStatus where
    parseJSON = parseJSONText "ConfigurationItemStatus"
