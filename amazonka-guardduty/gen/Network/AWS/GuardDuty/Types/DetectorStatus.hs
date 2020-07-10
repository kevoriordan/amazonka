{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.DetectorStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GuardDuty.Types.DetectorStatus (
  DetectorStatus (
    ..
    , DSDisabled
    , DSEnabled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data DetectorStatus = DetectorStatus' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern DSDisabled :: DetectorStatus
pattern DSDisabled = DetectorStatus' "DISABLED"

pattern DSEnabled :: DetectorStatus
pattern DSEnabled = DetectorStatus' "ENABLED"

{-# COMPLETE
  DSDisabled,
  DSEnabled,
  DetectorStatus' #-}

instance FromText DetectorStatus where
    parser = (DetectorStatus' . mk) <$> takeText

instance ToText DetectorStatus where
    toText (DetectorStatus' ci) = original ci

-- | Represents an enum of /known/ $DetectorStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DetectorStatus where
    toEnum i = case i of
        0 -> DSDisabled
        1 -> DSEnabled
        _ -> (error . showText) $ "Unknown index for DetectorStatus: " <> toText i
    fromEnum x = case x of
        DSDisabled -> 0
        DSEnabled -> 1
        DetectorStatus' name -> (error . showText) $ "Unknown DetectorStatus: " <> original name

-- | Represents the bounds of /known/ $DetectorStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DetectorStatus where
    minBound = DSDisabled
    maxBound = DSEnabled

instance Hashable     DetectorStatus
instance NFData       DetectorStatus
instance ToByteString DetectorStatus
instance ToQuery      DetectorStatus
instance ToHeader     DetectorStatus

instance FromJSON DetectorStatus where
    parseJSON = parseJSONText "DetectorStatus"
