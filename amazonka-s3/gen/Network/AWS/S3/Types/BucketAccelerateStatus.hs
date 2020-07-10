{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.BucketAccelerateStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.BucketAccelerateStatus (
  BucketAccelerateStatus (
    ..
    , BASEnabled
    , BASSuspended
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude
import Network.AWS.S3.Internal

data BucketAccelerateStatus = BucketAccelerateStatus' (CI
                                                         Text)
                                deriving (Eq, Ord, Read, Show, Data, Typeable,
                                          Generic)

pattern BASEnabled :: BucketAccelerateStatus
pattern BASEnabled = BucketAccelerateStatus' "Enabled"

pattern BASSuspended :: BucketAccelerateStatus
pattern BASSuspended = BucketAccelerateStatus' "Suspended"

{-# COMPLETE
  BASEnabled,
  BASSuspended,
  BucketAccelerateStatus' #-}

instance FromText BucketAccelerateStatus where
    parser = (BucketAccelerateStatus' . mk) <$> takeText

instance ToText BucketAccelerateStatus where
    toText (BucketAccelerateStatus' ci) = original ci

-- | Represents an enum of /known/ $BucketAccelerateStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum BucketAccelerateStatus where
    toEnum i = case i of
        0 -> BASEnabled
        1 -> BASSuspended
        _ -> (error . showText) $ "Unknown index for BucketAccelerateStatus: " <> toText i
    fromEnum x = case x of
        BASEnabled -> 0
        BASSuspended -> 1
        BucketAccelerateStatus' name -> (error . showText) $ "Unknown BucketAccelerateStatus: " <> original name

-- | Represents the bounds of /known/ $BucketAccelerateStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded BucketAccelerateStatus where
    minBound = BASEnabled
    maxBound = BASSuspended

instance Hashable     BucketAccelerateStatus
instance NFData       BucketAccelerateStatus
instance ToByteString BucketAccelerateStatus
instance ToQuery      BucketAccelerateStatus
instance ToHeader     BucketAccelerateStatus

instance FromXML BucketAccelerateStatus where
    parseXML = parseXMLText "BucketAccelerateStatus"

instance ToXML BucketAccelerateStatus where
    toXML = toXMLText
