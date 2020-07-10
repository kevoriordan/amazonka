{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.RegistrationStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFormation.Types.RegistrationStatus (
  RegistrationStatus (
    ..
    , RSComplete
    , RSFailed
    , RSInProgress
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data RegistrationStatus = RegistrationStatus' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern RSComplete :: RegistrationStatus
pattern RSComplete = RegistrationStatus' "COMPLETE"

pattern RSFailed :: RegistrationStatus
pattern RSFailed = RegistrationStatus' "FAILED"

pattern RSInProgress :: RegistrationStatus
pattern RSInProgress = RegistrationStatus' "IN_PROGRESS"

{-# COMPLETE
  RSComplete,
  RSFailed,
  RSInProgress,
  RegistrationStatus' #-}

instance FromText RegistrationStatus where
    parser = (RegistrationStatus' . mk) <$> takeText

instance ToText RegistrationStatus where
    toText (RegistrationStatus' ci) = original ci

-- | Represents an enum of /known/ $RegistrationStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum RegistrationStatus where
    toEnum i = case i of
        0 -> RSComplete
        1 -> RSFailed
        2 -> RSInProgress
        _ -> (error . showText) $ "Unknown index for RegistrationStatus: " <> toText i
    fromEnum x = case x of
        RSComplete -> 0
        RSFailed -> 1
        RSInProgress -> 2
        RegistrationStatus' name -> (error . showText) $ "Unknown RegistrationStatus: " <> original name

-- | Represents the bounds of /known/ $RegistrationStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded RegistrationStatus where
    minBound = RSComplete
    maxBound = RSInProgress

instance Hashable     RegistrationStatus
instance NFData       RegistrationStatus
instance ToByteString RegistrationStatus
instance ToQuery      RegistrationStatus
instance ToHeader     RegistrationStatus

instance FromXML RegistrationStatus where
    parseXML = parseXMLText "RegistrationStatus"
