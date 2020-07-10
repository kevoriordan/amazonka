{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Cloud9.Types.EnvironmentStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Cloud9.Types.EnvironmentStatus (
  EnvironmentStatus (
    ..
    , ESConnecting
    , ESCreating
    , ESDeleting
    , ESError'
    , ESReady
    , ESStopped
    , ESStopping
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data EnvironmentStatus = EnvironmentStatus' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern ESConnecting :: EnvironmentStatus
pattern ESConnecting = EnvironmentStatus' "connecting"

pattern ESCreating :: EnvironmentStatus
pattern ESCreating = EnvironmentStatus' "creating"

pattern ESDeleting :: EnvironmentStatus
pattern ESDeleting = EnvironmentStatus' "deleting"

pattern ESError' :: EnvironmentStatus
pattern ESError' = EnvironmentStatus' "error"

pattern ESReady :: EnvironmentStatus
pattern ESReady = EnvironmentStatus' "ready"

pattern ESStopped :: EnvironmentStatus
pattern ESStopped = EnvironmentStatus' "stopped"

pattern ESStopping :: EnvironmentStatus
pattern ESStopping = EnvironmentStatus' "stopping"

{-# COMPLETE
  ESConnecting,
  ESCreating,
  ESDeleting,
  ESError',
  ESReady,
  ESStopped,
  ESStopping,
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
        0 -> ESConnecting
        1 -> ESCreating
        2 -> ESDeleting
        3 -> ESError'
        4 -> ESReady
        5 -> ESStopped
        6 -> ESStopping
        _ -> (error . showText) $ "Unknown index for EnvironmentStatus: " <> toText i
    fromEnum x = case x of
        ESConnecting -> 0
        ESCreating -> 1
        ESDeleting -> 2
        ESError' -> 3
        ESReady -> 4
        ESStopped -> 5
        ESStopping -> 6
        EnvironmentStatus' name -> (error . showText) $ "Unknown EnvironmentStatus: " <> original name

-- | Represents the bounds of /known/ $EnvironmentStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded EnvironmentStatus where
    minBound = ESConnecting
    maxBound = ESStopping

instance Hashable     EnvironmentStatus
instance NFData       EnvironmentStatus
instance ToByteString EnvironmentStatus
instance ToQuery      EnvironmentStatus
instance ToHeader     EnvironmentStatus

instance FromJSON EnvironmentStatus where
    parseJSON = parseJSONText "EnvironmentStatus"
