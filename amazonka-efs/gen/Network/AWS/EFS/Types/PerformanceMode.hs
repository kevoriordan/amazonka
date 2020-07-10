{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.Types.PerformanceMode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EFS.Types.PerformanceMode (
  PerformanceMode (
    ..
    , GeneralPurpose
    , MaxIO
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data PerformanceMode = PerformanceMode' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern GeneralPurpose :: PerformanceMode
pattern GeneralPurpose = PerformanceMode' "generalPurpose"

pattern MaxIO :: PerformanceMode
pattern MaxIO = PerformanceMode' "maxIO"

{-# COMPLETE
  GeneralPurpose,
  MaxIO,
  PerformanceMode' #-}

instance FromText PerformanceMode where
    parser = (PerformanceMode' . mk) <$> takeText

instance ToText PerformanceMode where
    toText (PerformanceMode' ci) = original ci

-- | Represents an enum of /known/ $PerformanceMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum PerformanceMode where
    toEnum i = case i of
        0 -> GeneralPurpose
        1 -> MaxIO
        _ -> (error . showText) $ "Unknown index for PerformanceMode: " <> toText i
    fromEnum x = case x of
        GeneralPurpose -> 0
        MaxIO -> 1
        PerformanceMode' name -> (error . showText) $ "Unknown PerformanceMode: " <> original name

-- | Represents the bounds of /known/ $PerformanceMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded PerformanceMode where
    minBound = GeneralPurpose
    maxBound = MaxIO

instance Hashable     PerformanceMode
instance NFData       PerformanceMode
instance ToByteString PerformanceMode
instance ToQuery      PerformanceMode
instance ToHeader     PerformanceMode

instance ToJSON PerformanceMode where
    toJSON = toJSONText

instance FromJSON PerformanceMode where
    parseJSON = parseJSONText "PerformanceMode"
