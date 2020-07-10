{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.TeletextPageType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.TeletextPageType (
  TeletextPageType (
    ..
    , PageTypeAddlInfo
    , PageTypeHearingImpairedSubtitle
    , PageTypeInitial
    , PageTypeProgramSchedule
    , PageTypeSubtitle
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | A page type as defined in the standard ETSI EN 300 468, Table 94
data TeletextPageType = TeletextPageType' (CI Text)
                          deriving (Eq, Ord, Read, Show, Data, Typeable,
                                    Generic)

pattern PageTypeAddlInfo :: TeletextPageType
pattern PageTypeAddlInfo = TeletextPageType' "PAGE_TYPE_ADDL_INFO"

pattern PageTypeHearingImpairedSubtitle :: TeletextPageType
pattern PageTypeHearingImpairedSubtitle = TeletextPageType' "PAGE_TYPE_HEARING_IMPAIRED_SUBTITLE"

pattern PageTypeInitial :: TeletextPageType
pattern PageTypeInitial = TeletextPageType' "PAGE_TYPE_INITIAL"

pattern PageTypeProgramSchedule :: TeletextPageType
pattern PageTypeProgramSchedule = TeletextPageType' "PAGE_TYPE_PROGRAM_SCHEDULE"

pattern PageTypeSubtitle :: TeletextPageType
pattern PageTypeSubtitle = TeletextPageType' "PAGE_TYPE_SUBTITLE"

{-# COMPLETE
  PageTypeAddlInfo,
  PageTypeHearingImpairedSubtitle,
  PageTypeInitial,
  PageTypeProgramSchedule,
  PageTypeSubtitle,
  TeletextPageType' #-}

instance FromText TeletextPageType where
    parser = (TeletextPageType' . mk) <$> takeText

instance ToText TeletextPageType where
    toText (TeletextPageType' ci) = original ci

-- | Represents an enum of /known/ $TeletextPageType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum TeletextPageType where
    toEnum i = case i of
        0 -> PageTypeAddlInfo
        1 -> PageTypeHearingImpairedSubtitle
        2 -> PageTypeInitial
        3 -> PageTypeProgramSchedule
        4 -> PageTypeSubtitle
        _ -> (error . showText) $ "Unknown index for TeletextPageType: " <> toText i
    fromEnum x = case x of
        PageTypeAddlInfo -> 0
        PageTypeHearingImpairedSubtitle -> 1
        PageTypeInitial -> 2
        PageTypeProgramSchedule -> 3
        PageTypeSubtitle -> 4
        TeletextPageType' name -> (error . showText) $ "Unknown TeletextPageType: " <> original name

-- | Represents the bounds of /known/ $TeletextPageType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded TeletextPageType where
    minBound = PageTypeAddlInfo
    maxBound = PageTypeSubtitle

instance Hashable     TeletextPageType
instance NFData       TeletextPageType
instance ToByteString TeletextPageType
instance ToQuery      TeletextPageType
instance ToHeader     TeletextPageType

instance ToJSON TeletextPageType where
    toJSON = toJSONText

instance FromJSON TeletextPageType where
    parseJSON = parseJSONText "TeletextPageType"
