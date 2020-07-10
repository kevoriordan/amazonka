{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.S3DataDistribution
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.S3DataDistribution (
  S3DataDistribution (
    ..
    , FullyReplicated
    , ShardedByS3Key
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data S3DataDistribution = S3DataDistribution' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern FullyReplicated :: S3DataDistribution
pattern FullyReplicated = S3DataDistribution' "FullyReplicated"

pattern ShardedByS3Key :: S3DataDistribution
pattern ShardedByS3Key = S3DataDistribution' "ShardedByS3Key"

{-# COMPLETE
  FullyReplicated,
  ShardedByS3Key,
  S3DataDistribution' #-}

instance FromText S3DataDistribution where
    parser = (S3DataDistribution' . mk) <$> takeText

instance ToText S3DataDistribution where
    toText (S3DataDistribution' ci) = original ci

-- | Represents an enum of /known/ $S3DataDistribution.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum S3DataDistribution where
    toEnum i = case i of
        0 -> FullyReplicated
        1 -> ShardedByS3Key
        _ -> (error . showText) $ "Unknown index for S3DataDistribution: " <> toText i
    fromEnum x = case x of
        FullyReplicated -> 0
        ShardedByS3Key -> 1
        S3DataDistribution' name -> (error . showText) $ "Unknown S3DataDistribution: " <> original name

-- | Represents the bounds of /known/ $S3DataDistribution.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded S3DataDistribution where
    minBound = FullyReplicated
    maxBound = ShardedByS3Key

instance Hashable     S3DataDistribution
instance NFData       S3DataDistribution
instance ToByteString S3DataDistribution
instance ToQuery      S3DataDistribution
instance ToHeader     S3DataDistribution

instance ToJSON S3DataDistribution where
    toJSON = toJSONText

instance FromJSON S3DataDistribution where
    parseJSON = parseJSONText "S3DataDistribution"
