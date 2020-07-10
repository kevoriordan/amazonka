{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.PublishingStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GuardDuty.Types.PublishingStatus (
  PublishingStatus (
    ..
    , PendingVerification
    , Publishing
    , Stopped
    , UnableToPublishFixDestinationProperty
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data PublishingStatus = PublishingStatus' (CI Text)
                          deriving (Eq, Ord, Read, Show, Data, Typeable,
                                    Generic)

pattern PendingVerification :: PublishingStatus
pattern PendingVerification = PublishingStatus' "PENDING_VERIFICATION"

pattern Publishing :: PublishingStatus
pattern Publishing = PublishingStatus' "PUBLISHING"

pattern Stopped :: PublishingStatus
pattern Stopped = PublishingStatus' "STOPPED"

pattern UnableToPublishFixDestinationProperty :: PublishingStatus
pattern UnableToPublishFixDestinationProperty = PublishingStatus' "UNABLE_TO_PUBLISH_FIX_DESTINATION_PROPERTY"

{-# COMPLETE
  PendingVerification,
  Publishing,
  Stopped,
  UnableToPublishFixDestinationProperty,
  PublishingStatus' #-}

instance FromText PublishingStatus where
    parser = (PublishingStatus' . mk) <$> takeText

instance ToText PublishingStatus where
    toText (PublishingStatus' ci) = original ci

-- | Represents an enum of /known/ $PublishingStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum PublishingStatus where
    toEnum i = case i of
        0 -> PendingVerification
        1 -> Publishing
        2 -> Stopped
        3 -> UnableToPublishFixDestinationProperty
        _ -> (error . showText) $ "Unknown index for PublishingStatus: " <> toText i
    fromEnum x = case x of
        PendingVerification -> 0
        Publishing -> 1
        Stopped -> 2
        UnableToPublishFixDestinationProperty -> 3
        PublishingStatus' name -> (error . showText) $ "Unknown PublishingStatus: " <> original name

-- | Represents the bounds of /known/ $PublishingStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded PublishingStatus where
    minBound = PendingVerification
    maxBound = UnableToPublishFixDestinationProperty

instance Hashable     PublishingStatus
instance NFData       PublishingStatus
instance ToByteString PublishingStatus
instance ToQuery      PublishingStatus
instance ToHeader     PublishingStatus

instance FromJSON PublishingStatus where
    parseJSON = parseJSONText "PublishingStatus"
