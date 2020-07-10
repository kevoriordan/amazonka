{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.OutputGroupType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.OutputGroupType (
  OutputGroupType (
    ..
    , CmafGroupSettings
    , DashIsoGroupSettings
    , FileGroupSettings
    , HlsGroupSettings
    , MsSmoothGroupSettings
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Type of output group (File group, Apple HLS, DASH ISO, Microsoft Smooth Streaming, CMAF)
data OutputGroupType = OutputGroupType' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern CmafGroupSettings :: OutputGroupType
pattern CmafGroupSettings = OutputGroupType' "CMAF_GROUP_SETTINGS"

pattern DashIsoGroupSettings :: OutputGroupType
pattern DashIsoGroupSettings = OutputGroupType' "DASH_ISO_GROUP_SETTINGS"

pattern FileGroupSettings :: OutputGroupType
pattern FileGroupSettings = OutputGroupType' "FILE_GROUP_SETTINGS"

pattern HlsGroupSettings :: OutputGroupType
pattern HlsGroupSettings = OutputGroupType' "HLS_GROUP_SETTINGS"

pattern MsSmoothGroupSettings :: OutputGroupType
pattern MsSmoothGroupSettings = OutputGroupType' "MS_SMOOTH_GROUP_SETTINGS"

{-# COMPLETE
  CmafGroupSettings,
  DashIsoGroupSettings,
  FileGroupSettings,
  HlsGroupSettings,
  MsSmoothGroupSettings,
  OutputGroupType' #-}

instance FromText OutputGroupType where
    parser = (OutputGroupType' . mk) <$> takeText

instance ToText OutputGroupType where
    toText (OutputGroupType' ci) = original ci

-- | Represents an enum of /known/ $OutputGroupType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum OutputGroupType where
    toEnum i = case i of
        0 -> CmafGroupSettings
        1 -> DashIsoGroupSettings
        2 -> FileGroupSettings
        3 -> HlsGroupSettings
        4 -> MsSmoothGroupSettings
        _ -> (error . showText) $ "Unknown index for OutputGroupType: " <> toText i
    fromEnum x = case x of
        CmafGroupSettings -> 0
        DashIsoGroupSettings -> 1
        FileGroupSettings -> 2
        HlsGroupSettings -> 3
        MsSmoothGroupSettings -> 4
        OutputGroupType' name -> (error . showText) $ "Unknown OutputGroupType: " <> original name

-- | Represents the bounds of /known/ $OutputGroupType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded OutputGroupType where
    minBound = CmafGroupSettings
    maxBound = MsSmoothGroupSettings

instance Hashable     OutputGroupType
instance NFData       OutputGroupType
instance ToByteString OutputGroupType
instance ToQuery      OutputGroupType
instance ToHeader     OutputGroupType

instance ToJSON OutputGroupType where
    toJSON = toJSONText

instance FromJSON OutputGroupType where
    parseJSON = parseJSONText "OutputGroupType"
