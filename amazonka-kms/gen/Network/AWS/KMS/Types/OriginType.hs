{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Types.OriginType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KMS.Types.OriginType (
  OriginType (
    ..
    , AWSCloudhsm
    , AWSKMS
    , External
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data OriginType = OriginType' (CI Text)
                    deriving (Eq, Ord, Read, Show, Data, Typeable,
                              Generic)

pattern AWSCloudhsm :: OriginType
pattern AWSCloudhsm = OriginType' "AWS_CLOUDHSM"

pattern AWSKMS :: OriginType
pattern AWSKMS = OriginType' "AWS_KMS"

pattern External :: OriginType
pattern External = OriginType' "EXTERNAL"

{-# COMPLETE
  AWSCloudhsm,
  AWSKMS,
  External,
  OriginType' #-}

instance FromText OriginType where
    parser = (OriginType' . mk) <$> takeText

instance ToText OriginType where
    toText (OriginType' ci) = original ci

-- | Represents an enum of /known/ $OriginType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum OriginType where
    toEnum i = case i of
        0 -> AWSCloudhsm
        1 -> AWSKMS
        2 -> External
        _ -> (error . showText) $ "Unknown index for OriginType: " <> toText i
    fromEnum x = case x of
        AWSCloudhsm -> 0
        AWSKMS -> 1
        External -> 2
        OriginType' name -> (error . showText) $ "Unknown OriginType: " <> original name

-- | Represents the bounds of /known/ $OriginType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded OriginType where
    minBound = AWSCloudhsm
    maxBound = External

instance Hashable     OriginType
instance NFData       OriginType
instance ToByteString OriginType
instance ToQuery      OriginType
instance ToHeader     OriginType

instance ToJSON OriginType where
    toJSON = toJSONText

instance FromJSON OriginType where
    parseJSON = parseJSONText "OriginType"
