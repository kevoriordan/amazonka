{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Scte35DeviceRestrictions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.Scte35DeviceRestrictions (
  Scte35DeviceRestrictions (
    ..
    , SDRNone
    , SDRRestrictGROUP0
    , SDRRestrictGROUP1
    , SDRRestrictGROUP2
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Corresponds to the device_restrictions parameter in a segmentation_descriptor. If you include one of the "restriction" flags then you must include all four of them.
data Scte35DeviceRestrictions = Scte35DeviceRestrictions' (CI
                                                             Text)
                                  deriving (Eq, Ord, Read, Show, Data, Typeable,
                                            Generic)

pattern SDRNone :: Scte35DeviceRestrictions
pattern SDRNone = Scte35DeviceRestrictions' "NONE"

pattern SDRRestrictGROUP0 :: Scte35DeviceRestrictions
pattern SDRRestrictGROUP0 = Scte35DeviceRestrictions' "RESTRICT_GROUP0"

pattern SDRRestrictGROUP1 :: Scte35DeviceRestrictions
pattern SDRRestrictGROUP1 = Scte35DeviceRestrictions' "RESTRICT_GROUP1"

pattern SDRRestrictGROUP2 :: Scte35DeviceRestrictions
pattern SDRRestrictGROUP2 = Scte35DeviceRestrictions' "RESTRICT_GROUP2"

{-# COMPLETE
  SDRNone,
  SDRRestrictGROUP0,
  SDRRestrictGROUP1,
  SDRRestrictGROUP2,
  Scte35DeviceRestrictions' #-}

instance FromText Scte35DeviceRestrictions where
    parser = (Scte35DeviceRestrictions' . mk) <$> takeText

instance ToText Scte35DeviceRestrictions where
    toText (Scte35DeviceRestrictions' ci) = original ci

-- | Represents an enum of /known/ $Scte35DeviceRestrictions.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Scte35DeviceRestrictions where
    toEnum i = case i of
        0 -> SDRNone
        1 -> SDRRestrictGROUP0
        2 -> SDRRestrictGROUP1
        3 -> SDRRestrictGROUP2
        _ -> (error . showText) $ "Unknown index for Scte35DeviceRestrictions: " <> toText i
    fromEnum x = case x of
        SDRNone -> 0
        SDRRestrictGROUP0 -> 1
        SDRRestrictGROUP1 -> 2
        SDRRestrictGROUP2 -> 3
        Scte35DeviceRestrictions' name -> (error . showText) $ "Unknown Scte35DeviceRestrictions: " <> original name

-- | Represents the bounds of /known/ $Scte35DeviceRestrictions.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Scte35DeviceRestrictions where
    minBound = SDRNone
    maxBound = SDRRestrictGROUP2

instance Hashable     Scte35DeviceRestrictions
instance NFData       Scte35DeviceRestrictions
instance ToByteString Scte35DeviceRestrictions
instance ToQuery      Scte35DeviceRestrictions
instance ToHeader     Scte35DeviceRestrictions

instance ToJSON Scte35DeviceRestrictions where
    toJSON = toJSONText

instance FromJSON Scte35DeviceRestrictions where
    parseJSON = parseJSONText "Scte35DeviceRestrictions"
