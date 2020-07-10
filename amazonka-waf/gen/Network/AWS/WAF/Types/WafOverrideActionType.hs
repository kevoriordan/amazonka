{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.WafOverrideActionType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WAF.Types.WafOverrideActionType (
  WafOverrideActionType (
    ..
    , WOATCount
    , WOATNone
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data WafOverrideActionType = WafOverrideActionType' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern WOATCount :: WafOverrideActionType
pattern WOATCount = WafOverrideActionType' "COUNT"

pattern WOATNone :: WafOverrideActionType
pattern WOATNone = WafOverrideActionType' "NONE"

{-# COMPLETE
  WOATCount,
  WOATNone,
  WafOverrideActionType' #-}

instance FromText WafOverrideActionType where
    parser = (WafOverrideActionType' . mk) <$> takeText

instance ToText WafOverrideActionType where
    toText (WafOverrideActionType' ci) = original ci

-- | Represents an enum of /known/ $WafOverrideActionType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum WafOverrideActionType where
    toEnum i = case i of
        0 -> WOATCount
        1 -> WOATNone
        _ -> (error . showText) $ "Unknown index for WafOverrideActionType: " <> toText i
    fromEnum x = case x of
        WOATCount -> 0
        WOATNone -> 1
        WafOverrideActionType' name -> (error . showText) $ "Unknown WafOverrideActionType: " <> original name

-- | Represents the bounds of /known/ $WafOverrideActionType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded WafOverrideActionType where
    minBound = WOATCount
    maxBound = WOATNone

instance Hashable     WafOverrideActionType
instance NFData       WafOverrideActionType
instance ToByteString WafOverrideActionType
instance ToQuery      WafOverrideActionType
instance ToHeader     WafOverrideActionType

instance ToJSON WafOverrideActionType where
    toJSON = toJSONText

instance FromJSON WafOverrideActionType where
    parseJSON = parseJSONText "WafOverrideActionType"
