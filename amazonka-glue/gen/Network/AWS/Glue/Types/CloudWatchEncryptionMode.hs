{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.CloudWatchEncryptionMode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.CloudWatchEncryptionMode (
  CloudWatchEncryptionMode (
    ..
    , CWEMDisabled
    , CWEMSseKMS
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data CloudWatchEncryptionMode = CloudWatchEncryptionMode' (CI
                                                             Text)
                                  deriving (Eq, Ord, Read, Show, Data, Typeable,
                                            Generic)

pattern CWEMDisabled :: CloudWatchEncryptionMode
pattern CWEMDisabled = CloudWatchEncryptionMode' "DISABLED"

pattern CWEMSseKMS :: CloudWatchEncryptionMode
pattern CWEMSseKMS = CloudWatchEncryptionMode' "SSE-KMS"

{-# COMPLETE
  CWEMDisabled,
  CWEMSseKMS,
  CloudWatchEncryptionMode' #-}

instance FromText CloudWatchEncryptionMode where
    parser = (CloudWatchEncryptionMode' . mk) <$> takeText

instance ToText CloudWatchEncryptionMode where
    toText (CloudWatchEncryptionMode' ci) = original ci

-- | Represents an enum of /known/ $CloudWatchEncryptionMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum CloudWatchEncryptionMode where
    toEnum i = case i of
        0 -> CWEMDisabled
        1 -> CWEMSseKMS
        _ -> (error . showText) $ "Unknown index for CloudWatchEncryptionMode: " <> toText i
    fromEnum x = case x of
        CWEMDisabled -> 0
        CWEMSseKMS -> 1
        CloudWatchEncryptionMode' name -> (error . showText) $ "Unknown CloudWatchEncryptionMode: " <> original name

-- | Represents the bounds of /known/ $CloudWatchEncryptionMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded CloudWatchEncryptionMode where
    minBound = CWEMDisabled
    maxBound = CWEMSseKMS

instance Hashable     CloudWatchEncryptionMode
instance NFData       CloudWatchEncryptionMode
instance ToByteString CloudWatchEncryptionMode
instance ToQuery      CloudWatchEncryptionMode
instance ToHeader     CloudWatchEncryptionMode

instance ToJSON CloudWatchEncryptionMode where
    toJSON = toJSONText

instance FromJSON CloudWatchEncryptionMode where
    parseJSON = parseJSONText "CloudWatchEncryptionMode"
