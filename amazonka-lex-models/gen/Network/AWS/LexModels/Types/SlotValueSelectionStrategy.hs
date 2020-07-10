{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.SlotValueSelectionStrategy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.LexModels.Types.SlotValueSelectionStrategy (
  SlotValueSelectionStrategy (
    ..
    , OriginalValue
    , TopResolution
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data SlotValueSelectionStrategy = SlotValueSelectionStrategy' (CI
                                                                 Text)
                                    deriving (Eq, Ord, Read, Show, Data,
                                              Typeable, Generic)

pattern OriginalValue :: SlotValueSelectionStrategy
pattern OriginalValue = SlotValueSelectionStrategy' "ORIGINAL_VALUE"

pattern TopResolution :: SlotValueSelectionStrategy
pattern TopResolution = SlotValueSelectionStrategy' "TOP_RESOLUTION"

{-# COMPLETE
  OriginalValue,
  TopResolution,
  SlotValueSelectionStrategy' #-}

instance FromText SlotValueSelectionStrategy where
    parser = (SlotValueSelectionStrategy' . mk) <$> takeText

instance ToText SlotValueSelectionStrategy where
    toText (SlotValueSelectionStrategy' ci) = original ci

-- | Represents an enum of /known/ $SlotValueSelectionStrategy.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum SlotValueSelectionStrategy where
    toEnum i = case i of
        0 -> OriginalValue
        1 -> TopResolution
        _ -> (error . showText) $ "Unknown index for SlotValueSelectionStrategy: " <> toText i
    fromEnum x = case x of
        OriginalValue -> 0
        TopResolution -> 1
        SlotValueSelectionStrategy' name -> (error . showText) $ "Unknown SlotValueSelectionStrategy: " <> original name

-- | Represents the bounds of /known/ $SlotValueSelectionStrategy.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded SlotValueSelectionStrategy where
    minBound = OriginalValue
    maxBound = TopResolution

instance Hashable     SlotValueSelectionStrategy
instance NFData       SlotValueSelectionStrategy
instance ToByteString SlotValueSelectionStrategy
instance ToQuery      SlotValueSelectionStrategy
instance ToHeader     SlotValueSelectionStrategy

instance ToJSON SlotValueSelectionStrategy where
    toJSON = toJSONText

instance FromJSON SlotValueSelectionStrategy where
    parseJSON = parseJSONText "SlotValueSelectionStrategy"
