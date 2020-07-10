{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ObjectiveStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.ObjectiveStatus (
  ObjectiveStatus (
    ..
    , OSFailed
    , OSPending
    , OSSucceeded
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ObjectiveStatus = ObjectiveStatus' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern OSFailed :: ObjectiveStatus
pattern OSFailed = ObjectiveStatus' "Failed"

pattern OSPending :: ObjectiveStatus
pattern OSPending = ObjectiveStatus' "Pending"

pattern OSSucceeded :: ObjectiveStatus
pattern OSSucceeded = ObjectiveStatus' "Succeeded"

{-# COMPLETE
  OSFailed,
  OSPending,
  OSSucceeded,
  ObjectiveStatus' #-}

instance FromText ObjectiveStatus where
    parser = (ObjectiveStatus' . mk) <$> takeText

instance ToText ObjectiveStatus where
    toText (ObjectiveStatus' ci) = original ci

-- | Represents an enum of /known/ $ObjectiveStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ObjectiveStatus where
    toEnum i = case i of
        0 -> OSFailed
        1 -> OSPending
        2 -> OSSucceeded
        _ -> (error . showText) $ "Unknown index for ObjectiveStatus: " <> toText i
    fromEnum x = case x of
        OSFailed -> 0
        OSPending -> 1
        OSSucceeded -> 2
        ObjectiveStatus' name -> (error . showText) $ "Unknown ObjectiveStatus: " <> original name

-- | Represents the bounds of /known/ $ObjectiveStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ObjectiveStatus where
    minBound = OSFailed
    maxBound = OSSucceeded

instance Hashable     ObjectiveStatus
instance NFData       ObjectiveStatus
instance ToByteString ObjectiveStatus
instance ToQuery      ObjectiveStatus
instance ToHeader     ObjectiveStatus

instance FromJSON ObjectiveStatus where
    parseJSON = parseJSONText "ObjectiveStatus"
