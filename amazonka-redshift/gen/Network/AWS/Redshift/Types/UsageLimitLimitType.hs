{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.UsageLimitLimitType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Redshift.Types.UsageLimitLimitType (
  UsageLimitLimitType (
    ..
    , ULLTDataScanned
    , ULLTTime
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude
import Network.AWS.Redshift.Internal

data UsageLimitLimitType = UsageLimitLimitType' (CI
                                                   Text)
                             deriving (Eq, Ord, Read, Show, Data, Typeable,
                                       Generic)

pattern ULLTDataScanned :: UsageLimitLimitType
pattern ULLTDataScanned = UsageLimitLimitType' "data-scanned"

pattern ULLTTime :: UsageLimitLimitType
pattern ULLTTime = UsageLimitLimitType' "time"

{-# COMPLETE
  ULLTDataScanned,
  ULLTTime,
  UsageLimitLimitType' #-}

instance FromText UsageLimitLimitType where
    parser = (UsageLimitLimitType' . mk) <$> takeText

instance ToText UsageLimitLimitType where
    toText (UsageLimitLimitType' ci) = original ci

-- | Represents an enum of /known/ $UsageLimitLimitType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum UsageLimitLimitType where
    toEnum i = case i of
        0 -> ULLTDataScanned
        1 -> ULLTTime
        _ -> (error . showText) $ "Unknown index for UsageLimitLimitType: " <> toText i
    fromEnum x = case x of
        ULLTDataScanned -> 0
        ULLTTime -> 1
        UsageLimitLimitType' name -> (error . showText) $ "Unknown UsageLimitLimitType: " <> original name

-- | Represents the bounds of /known/ $UsageLimitLimitType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded UsageLimitLimitType where
    minBound = ULLTDataScanned
    maxBound = ULLTTime

instance Hashable     UsageLimitLimitType
instance NFData       UsageLimitLimitType
instance ToByteString UsageLimitLimitType
instance ToQuery      UsageLimitLimitType
instance ToHeader     UsageLimitLimitType

instance FromXML UsageLimitLimitType where
    parseXML = parseXMLText "UsageLimitLimitType"
