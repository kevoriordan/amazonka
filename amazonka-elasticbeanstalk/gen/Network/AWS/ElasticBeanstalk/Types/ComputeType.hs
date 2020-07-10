{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.ComputeType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticBeanstalk.Types.ComputeType (
  ComputeType (
    ..
    , BuildGENERAL1Large
    , BuildGENERAL1Medium
    , BuildGENERAL1Small
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ComputeType = ComputeType' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern BuildGENERAL1Large :: ComputeType
pattern BuildGENERAL1Large = ComputeType' "BUILD_GENERAL1_LARGE"

pattern BuildGENERAL1Medium :: ComputeType
pattern BuildGENERAL1Medium = ComputeType' "BUILD_GENERAL1_MEDIUM"

pattern BuildGENERAL1Small :: ComputeType
pattern BuildGENERAL1Small = ComputeType' "BUILD_GENERAL1_SMALL"

{-# COMPLETE
  BuildGENERAL1Large,
  BuildGENERAL1Medium,
  BuildGENERAL1Small,
  ComputeType' #-}

instance FromText ComputeType where
    parser = (ComputeType' . mk) <$> takeText

instance ToText ComputeType where
    toText (ComputeType' ci) = original ci

-- | Represents an enum of /known/ $ComputeType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ComputeType where
    toEnum i = case i of
        0 -> BuildGENERAL1Large
        1 -> BuildGENERAL1Medium
        2 -> BuildGENERAL1Small
        _ -> (error . showText) $ "Unknown index for ComputeType: " <> toText i
    fromEnum x = case x of
        BuildGENERAL1Large -> 0
        BuildGENERAL1Medium -> 1
        BuildGENERAL1Small -> 2
        ComputeType' name -> (error . showText) $ "Unknown ComputeType: " <> original name

-- | Represents the bounds of /known/ $ComputeType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ComputeType where
    minBound = BuildGENERAL1Large
    maxBound = BuildGENERAL1Small

instance Hashable     ComputeType
instance NFData       ComputeType
instance ToByteString ComputeType
instance ToQuery      ComputeType
instance ToHeader     ComputeType
