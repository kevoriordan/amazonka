{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.EnrollmentStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AlexaBusiness.Types.EnrollmentStatus (
  EnrollmentStatus (
    ..
    , ESDeregistering
    , ESDisassociating
    , ESInitialized
    , ESPending
    , ESRegistered
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data EnrollmentStatus = EnrollmentStatus' (CI Text)
                          deriving (Eq, Ord, Read, Show, Data, Typeable,
                                    Generic)

pattern ESDeregistering :: EnrollmentStatus
pattern ESDeregistering = EnrollmentStatus' "DEREGISTERING"

pattern ESDisassociating :: EnrollmentStatus
pattern ESDisassociating = EnrollmentStatus' "DISASSOCIATING"

pattern ESInitialized :: EnrollmentStatus
pattern ESInitialized = EnrollmentStatus' "INITIALIZED"

pattern ESPending :: EnrollmentStatus
pattern ESPending = EnrollmentStatus' "PENDING"

pattern ESRegistered :: EnrollmentStatus
pattern ESRegistered = EnrollmentStatus' "REGISTERED"

{-# COMPLETE
  ESDeregistering,
  ESDisassociating,
  ESInitialized,
  ESPending,
  ESRegistered,
  EnrollmentStatus' #-}

instance FromText EnrollmentStatus where
    parser = (EnrollmentStatus' . mk) <$> takeText

instance ToText EnrollmentStatus where
    toText (EnrollmentStatus' ci) = original ci

-- | Represents an enum of /known/ $EnrollmentStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum EnrollmentStatus where
    toEnum i = case i of
        0 -> ESDeregistering
        1 -> ESDisassociating
        2 -> ESInitialized
        3 -> ESPending
        4 -> ESRegistered
        _ -> (error . showText) $ "Unknown index for EnrollmentStatus: " <> toText i
    fromEnum x = case x of
        ESDeregistering -> 0
        ESDisassociating -> 1
        ESInitialized -> 2
        ESPending -> 3
        ESRegistered -> 4
        EnrollmentStatus' name -> (error . showText) $ "Unknown EnrollmentStatus: " <> original name

-- | Represents the bounds of /known/ $EnrollmentStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded EnrollmentStatus where
    minBound = ESDeregistering
    maxBound = ESRegistered

instance Hashable     EnrollmentStatus
instance NFData       EnrollmentStatus
instance ToByteString EnrollmentStatus
instance ToQuery      EnrollmentStatus
instance ToHeader     EnrollmentStatus

instance FromJSON EnrollmentStatus where
    parseJSON = parseJSONText "EnrollmentStatus"
