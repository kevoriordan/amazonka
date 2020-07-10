{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.LogTargetType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.LogTargetType (
  LogTargetType (
    ..
    , LTTDefault
    , LTTThingGroup
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data LogTargetType = LogTargetType' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern LTTDefault :: LogTargetType
pattern LTTDefault = LogTargetType' "DEFAULT"

pattern LTTThingGroup :: LogTargetType
pattern LTTThingGroup = LogTargetType' "THING_GROUP"

{-# COMPLETE
  LTTDefault,
  LTTThingGroup,
  LogTargetType' #-}

instance FromText LogTargetType where
    parser = (LogTargetType' . mk) <$> takeText

instance ToText LogTargetType where
    toText (LogTargetType' ci) = original ci

-- | Represents an enum of /known/ $LogTargetType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum LogTargetType where
    toEnum i = case i of
        0 -> LTTDefault
        1 -> LTTThingGroup
        _ -> (error . showText) $ "Unknown index for LogTargetType: " <> toText i
    fromEnum x = case x of
        LTTDefault -> 0
        LTTThingGroup -> 1
        LogTargetType' name -> (error . showText) $ "Unknown LogTargetType: " <> original name

-- | Represents the bounds of /known/ $LogTargetType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded LogTargetType where
    minBound = LTTDefault
    maxBound = LTTThingGroup

instance Hashable     LogTargetType
instance NFData       LogTargetType
instance ToByteString LogTargetType
instance ToQuery      LogTargetType
instance ToHeader     LogTargetType

instance ToJSON LogTargetType where
    toJSON = toJSONText

instance FromJSON LogTargetType where
    parseJSON = parseJSONText "LogTargetType"
