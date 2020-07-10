{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ObjectCannedACL
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.ObjectCannedACL (
  ObjectCannedACL (
    ..
    , OAWSExecRead
    , OAuthenticatedRead
    , OBucketOwnerFullControl
    , OBucketOwnerRead
    , OPrivate
    , OPublicRead
    , OPublicReadWrite
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude
import Network.AWS.S3.Internal

data ObjectCannedACL = ObjectCannedACL' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern OAWSExecRead :: ObjectCannedACL
pattern OAWSExecRead = ObjectCannedACL' "aws-exec-read"

pattern OAuthenticatedRead :: ObjectCannedACL
pattern OAuthenticatedRead = ObjectCannedACL' "authenticated-read"

pattern OBucketOwnerFullControl :: ObjectCannedACL
pattern OBucketOwnerFullControl = ObjectCannedACL' "bucket-owner-full-control"

pattern OBucketOwnerRead :: ObjectCannedACL
pattern OBucketOwnerRead = ObjectCannedACL' "bucket-owner-read"

pattern OPrivate :: ObjectCannedACL
pattern OPrivate = ObjectCannedACL' "private"

pattern OPublicRead :: ObjectCannedACL
pattern OPublicRead = ObjectCannedACL' "public-read"

pattern OPublicReadWrite :: ObjectCannedACL
pattern OPublicReadWrite = ObjectCannedACL' "public-read-write"

{-# COMPLETE
  OAWSExecRead,
  OAuthenticatedRead,
  OBucketOwnerFullControl,
  OBucketOwnerRead,
  OPrivate,
  OPublicRead,
  OPublicReadWrite,
  ObjectCannedACL' #-}

instance FromText ObjectCannedACL where
    parser = (ObjectCannedACL' . mk) <$> takeText

instance ToText ObjectCannedACL where
    toText (ObjectCannedACL' ci) = original ci

-- | Represents an enum of /known/ $ObjectCannedACL.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ObjectCannedACL where
    toEnum i = case i of
        0 -> OAWSExecRead
        1 -> OAuthenticatedRead
        2 -> OBucketOwnerFullControl
        3 -> OBucketOwnerRead
        4 -> OPrivate
        5 -> OPublicRead
        6 -> OPublicReadWrite
        _ -> (error . showText) $ "Unknown index for ObjectCannedACL: " <> toText i
    fromEnum x = case x of
        OAWSExecRead -> 0
        OAuthenticatedRead -> 1
        OBucketOwnerFullControl -> 2
        OBucketOwnerRead -> 3
        OPrivate -> 4
        OPublicRead -> 5
        OPublicReadWrite -> 6
        ObjectCannedACL' name -> (error . showText) $ "Unknown ObjectCannedACL: " <> original name

-- | Represents the bounds of /known/ $ObjectCannedACL.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ObjectCannedACL where
    minBound = OAWSExecRead
    maxBound = OPublicReadWrite

instance Hashable     ObjectCannedACL
instance NFData       ObjectCannedACL
instance ToByteString ObjectCannedACL
instance ToQuery      ObjectCannedACL
instance ToHeader     ObjectCannedACL

instance ToXML ObjectCannedACL where
    toXML = toXMLText
