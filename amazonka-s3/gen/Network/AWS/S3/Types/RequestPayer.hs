{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.RequestPayer
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.RequestPayer (
  RequestPayer (
    ..
    , RPRequester
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude
import Network.AWS.S3.Internal

-- | Confirms that the requester knows that they will be charged for the request. Bucket owners need not specify this parameter in their requests. For information about downloading objects from requester pays buckets, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/ObjectsinRequesterPaysBuckets.html Downloading Objects in Requestor Pays Buckets> in the /Amazon S3 Developer Guide/ .
--
--
data RequestPayer = RequestPayer' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern RPRequester :: RequestPayer
pattern RPRequester = RequestPayer' "requester"

{-# COMPLETE
  RPRequester,
  RequestPayer' #-}

instance FromText RequestPayer where
    parser = (RequestPayer' . mk) <$> takeText

instance ToText RequestPayer where
    toText (RequestPayer' ci) = original ci

-- | Represents an enum of /known/ $RequestPayer.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum RequestPayer where
    toEnum i = case i of
        0 -> RPRequester
        _ -> (error . showText) $ "Unknown index for RequestPayer: " <> toText i
    fromEnum x = case x of
        RPRequester -> 0
        RequestPayer' name -> (error . showText) $ "Unknown RequestPayer: " <> original name

-- | Represents the bounds of /known/ $RequestPayer.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded RequestPayer where
    minBound = RPRequester
    maxBound = RPRequester

instance Hashable     RequestPayer
instance NFData       RequestPayer
instance ToByteString RequestPayer
instance ToQuery      RequestPayer
instance ToHeader     RequestPayer

instance ToXML RequestPayer where
    toXML = toXMLText
