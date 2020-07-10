{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.CannedAccessControlList
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.CannedAccessControlList (
  CannedAccessControlList (
    ..
    , AWSExecRead
    , AuthenticatedRead
    , BucketOwnerFullControl
    , BucketOwnerRead
    , LogDeliveryWrite
    , Private
    , PublicRead
    , PublicReadWrite
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data CannedAccessControlList = CannedAccessControlList' (CI
                                                           Text)
                                 deriving (Eq, Ord, Read, Show, Data, Typeable,
                                           Generic)

pattern AWSExecRead :: CannedAccessControlList
pattern AWSExecRead = CannedAccessControlList' "aws-exec-read"

pattern AuthenticatedRead :: CannedAccessControlList
pattern AuthenticatedRead = CannedAccessControlList' "authenticated-read"

pattern BucketOwnerFullControl :: CannedAccessControlList
pattern BucketOwnerFullControl = CannedAccessControlList' "bucket-owner-full-control"

pattern BucketOwnerRead :: CannedAccessControlList
pattern BucketOwnerRead = CannedAccessControlList' "bucket-owner-read"

pattern LogDeliveryWrite :: CannedAccessControlList
pattern LogDeliveryWrite = CannedAccessControlList' "log-delivery-write"

pattern Private :: CannedAccessControlList
pattern Private = CannedAccessControlList' "private"

pattern PublicRead :: CannedAccessControlList
pattern PublicRead = CannedAccessControlList' "public-read"

pattern PublicReadWrite :: CannedAccessControlList
pattern PublicReadWrite = CannedAccessControlList' "public-read-write"

{-# COMPLETE
  AWSExecRead,
  AuthenticatedRead,
  BucketOwnerFullControl,
  BucketOwnerRead,
  LogDeliveryWrite,
  Private,
  PublicRead,
  PublicReadWrite,
  CannedAccessControlList' #-}

instance FromText CannedAccessControlList where
    parser = (CannedAccessControlList' . mk) <$> takeText

instance ToText CannedAccessControlList where
    toText (CannedAccessControlList' ci) = original ci

-- | Represents an enum of /known/ $CannedAccessControlList.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum CannedAccessControlList where
    toEnum i = case i of
        0 -> AWSExecRead
        1 -> AuthenticatedRead
        2 -> BucketOwnerFullControl
        3 -> BucketOwnerRead
        4 -> LogDeliveryWrite
        5 -> Private
        6 -> PublicRead
        7 -> PublicReadWrite
        _ -> (error . showText) $ "Unknown index for CannedAccessControlList: " <> toText i
    fromEnum x = case x of
        AWSExecRead -> 0
        AuthenticatedRead -> 1
        BucketOwnerFullControl -> 2
        BucketOwnerRead -> 3
        LogDeliveryWrite -> 4
        Private -> 5
        PublicRead -> 6
        PublicReadWrite -> 7
        CannedAccessControlList' name -> (error . showText) $ "Unknown CannedAccessControlList: " <> original name

-- | Represents the bounds of /known/ $CannedAccessControlList.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded CannedAccessControlList where
    minBound = AWSExecRead
    maxBound = PublicReadWrite

instance Hashable     CannedAccessControlList
instance NFData       CannedAccessControlList
instance ToByteString CannedAccessControlList
instance ToQuery      CannedAccessControlList
instance ToHeader     CannedAccessControlList

instance ToJSON CannedAccessControlList where
    toJSON = toJSONText

instance FromJSON CannedAccessControlList where
    parseJSON = parseJSONText "CannedAccessControlList"
