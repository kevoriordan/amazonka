{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.ICPRecordalStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types.ICPRecordalStatus (
  ICPRecordalStatus (
    ..
    , Approved
    , Pending
    , Suspended
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ICPRecordalStatus = ICPRecordalStatus' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern Approved :: ICPRecordalStatus
pattern Approved = ICPRecordalStatus' "APPROVED"

pattern Pending :: ICPRecordalStatus
pattern Pending = ICPRecordalStatus' "PENDING"

pattern Suspended :: ICPRecordalStatus
pattern Suspended = ICPRecordalStatus' "SUSPENDED"

{-# COMPLETE
  Approved,
  Pending,
  Suspended,
  ICPRecordalStatus' #-}

instance FromText ICPRecordalStatus where
    parser = (ICPRecordalStatus' . mk) <$> takeText

instance ToText ICPRecordalStatus where
    toText (ICPRecordalStatus' ci) = original ci

-- | Represents an enum of /known/ $ICPRecordalStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ICPRecordalStatus where
    toEnum i = case i of
        0 -> Approved
        1 -> Pending
        2 -> Suspended
        _ -> (error . showText) $ "Unknown index for ICPRecordalStatus: " <> toText i
    fromEnum x = case x of
        Approved -> 0
        Pending -> 1
        Suspended -> 2
        ICPRecordalStatus' name -> (error . showText) $ "Unknown ICPRecordalStatus: " <> original name

-- | Represents the bounds of /known/ $ICPRecordalStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ICPRecordalStatus where
    minBound = Approved
    maxBound = Suspended

instance Hashable     ICPRecordalStatus
instance NFData       ICPRecordalStatus
instance ToByteString ICPRecordalStatus
instance ToQuery      ICPRecordalStatus
instance ToHeader     ICPRecordalStatus

instance FromXML ICPRecordalStatus where
    parseXML = parseXMLText "ICPRecordalStatus"
