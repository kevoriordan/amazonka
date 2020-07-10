{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.ObjectACL
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.StorageGateway.Types.ObjectACL (
  ObjectACL (
    ..
    , AWSExecRead
    , AuthenticatedRead
    , BucketOwnerFullControl
    , BucketOwnerRead
    , Private
    , PublicRead
    , PublicReadWrite
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | A value that sets the access control list permission for objects in the S3 bucket that a file gateway puts objects into. The default value is "private".
--
--
data ObjectACL = ObjectACL' (CI Text)
                   deriving (Eq, Ord, Read, Show, Data, Typeable,
                             Generic)

pattern AWSExecRead :: ObjectACL
pattern AWSExecRead = ObjectACL' "aws-exec-read"

pattern AuthenticatedRead :: ObjectACL
pattern AuthenticatedRead = ObjectACL' "authenticated-read"

pattern BucketOwnerFullControl :: ObjectACL
pattern BucketOwnerFullControl = ObjectACL' "bucket-owner-full-control"

pattern BucketOwnerRead :: ObjectACL
pattern BucketOwnerRead = ObjectACL' "bucket-owner-read"

pattern Private :: ObjectACL
pattern Private = ObjectACL' "private"

pattern PublicRead :: ObjectACL
pattern PublicRead = ObjectACL' "public-read"

pattern PublicReadWrite :: ObjectACL
pattern PublicReadWrite = ObjectACL' "public-read-write"

{-# COMPLETE
  AWSExecRead,
  AuthenticatedRead,
  BucketOwnerFullControl,
  BucketOwnerRead,
  Private,
  PublicRead,
  PublicReadWrite,
  ObjectACL' #-}

instance FromText ObjectACL where
    parser = (ObjectACL' . mk) <$> takeText

instance ToText ObjectACL where
    toText (ObjectACL' ci) = original ci

-- | Represents an enum of /known/ $ObjectACL.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ObjectACL where
    toEnum i = case i of
        0 -> AWSExecRead
        1 -> AuthenticatedRead
        2 -> BucketOwnerFullControl
        3 -> BucketOwnerRead
        4 -> Private
        5 -> PublicRead
        6 -> PublicReadWrite
        _ -> (error . showText) $ "Unknown index for ObjectACL: " <> toText i
    fromEnum x = case x of
        AWSExecRead -> 0
        AuthenticatedRead -> 1
        BucketOwnerFullControl -> 2
        BucketOwnerRead -> 3
        Private -> 4
        PublicRead -> 5
        PublicReadWrite -> 6
        ObjectACL' name -> (error . showText) $ "Unknown ObjectACL: " <> original name

-- | Represents the bounds of /known/ $ObjectACL.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ObjectACL where
    minBound = AWSExecRead
    maxBound = PublicReadWrite

instance Hashable     ObjectACL
instance NFData       ObjectACL
instance ToByteString ObjectACL
instance ToQuery      ObjectACL
instance ToHeader     ObjectACL

instance ToJSON ObjectACL where
    toJSON = toJSONText

instance FromJSON ObjectACL where
    parseJSON = parseJSONText "ObjectACL"
