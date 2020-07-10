{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.RadiusStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DirectoryService.Types.RadiusStatus (
  RadiusStatus (
    ..
    , RSCompleted
    , RSCreating
    , RSFailed
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data RadiusStatus = RadiusStatus' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern RSCompleted :: RadiusStatus
pattern RSCompleted = RadiusStatus' "Completed"

pattern RSCreating :: RadiusStatus
pattern RSCreating = RadiusStatus' "Creating"

pattern RSFailed :: RadiusStatus
pattern RSFailed = RadiusStatus' "Failed"

{-# COMPLETE
  RSCompleted,
  RSCreating,
  RSFailed,
  RadiusStatus' #-}

instance FromText RadiusStatus where
    parser = (RadiusStatus' . mk) <$> takeText

instance ToText RadiusStatus where
    toText (RadiusStatus' ci) = original ci

-- | Represents an enum of /known/ $RadiusStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum RadiusStatus where
    toEnum i = case i of
        0 -> RSCompleted
        1 -> RSCreating
        2 -> RSFailed
        _ -> (error . showText) $ "Unknown index for RadiusStatus: " <> toText i
    fromEnum x = case x of
        RSCompleted -> 0
        RSCreating -> 1
        RSFailed -> 2
        RadiusStatus' name -> (error . showText) $ "Unknown RadiusStatus: " <> original name

-- | Represents the bounds of /known/ $RadiusStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded RadiusStatus where
    minBound = RSCompleted
    maxBound = RSFailed

instance Hashable     RadiusStatus
instance NFData       RadiusStatus
instance ToByteString RadiusStatus
instance ToQuery      RadiusStatus
instance ToHeader     RadiusStatus

instance FromJSON RadiusStatus where
    parseJSON = parseJSONText "RadiusStatus"
