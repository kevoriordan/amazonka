{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types.CannedACL
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glacier.Types.CannedACL (
  CannedACL (
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

data CannedACL = CannedACL' (CI Text)
                   deriving (Eq, Ord, Read, Show, Data, Typeable,
                             Generic)

pattern AWSExecRead :: CannedACL
pattern AWSExecRead = CannedACL' "aws-exec-read"

pattern AuthenticatedRead :: CannedACL
pattern AuthenticatedRead = CannedACL' "authenticated-read"

pattern BucketOwnerFullControl :: CannedACL
pattern BucketOwnerFullControl = CannedACL' "bucket-owner-full-control"

pattern BucketOwnerRead :: CannedACL
pattern BucketOwnerRead = CannedACL' "bucket-owner-read"

pattern Private :: CannedACL
pattern Private = CannedACL' "private"

pattern PublicRead :: CannedACL
pattern PublicRead = CannedACL' "public-read"

pattern PublicReadWrite :: CannedACL
pattern PublicReadWrite = CannedACL' "public-read-write"

{-# COMPLETE
  AWSExecRead,
  AuthenticatedRead,
  BucketOwnerFullControl,
  BucketOwnerRead,
  Private,
  PublicRead,
  PublicReadWrite,
  CannedACL' #-}

instance FromText CannedACL where
    parser = (CannedACL' . mk) <$> takeText

instance ToText CannedACL where
    toText (CannedACL' ci) = original ci

-- | Represents an enum of /known/ $CannedACL.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum CannedACL where
    toEnum i = case i of
        0 -> AWSExecRead
        1 -> AuthenticatedRead
        2 -> BucketOwnerFullControl
        3 -> BucketOwnerRead
        4 -> Private
        5 -> PublicRead
        6 -> PublicReadWrite
        _ -> (error . showText) $ "Unknown index for CannedACL: " <> toText i
    fromEnum x = case x of
        AWSExecRead -> 0
        AuthenticatedRead -> 1
        BucketOwnerFullControl -> 2
        BucketOwnerRead -> 3
        Private -> 4
        PublicRead -> 5
        PublicReadWrite -> 6
        CannedACL' name -> (error . showText) $ "Unknown CannedACL: " <> original name

-- | Represents the bounds of /known/ $CannedACL.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded CannedACL where
    minBound = AWSExecRead
    maxBound = PublicReadWrite

instance Hashable     CannedACL
instance NFData       CannedACL
instance ToByteString CannedACL
instance ToQuery      CannedACL
instance ToHeader     CannedACL

instance ToJSON CannedACL where
    toJSON = toJSONText

instance FromJSON CannedACL where
    parseJSON = parseJSONText "CannedACL"
