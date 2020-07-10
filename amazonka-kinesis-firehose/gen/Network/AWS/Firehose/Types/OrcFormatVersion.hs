{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.OrcFormatVersion
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Firehose.Types.OrcFormatVersion (
  OrcFormatVersion (
    ..
    , V011
    , V012
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data OrcFormatVersion = OrcFormatVersion' (CI Text)
                          deriving (Eq, Ord, Read, Show, Data, Typeable,
                                    Generic)

pattern V011 :: OrcFormatVersion
pattern V011 = OrcFormatVersion' "V0_11"

pattern V012 :: OrcFormatVersion
pattern V012 = OrcFormatVersion' "V0_12"

{-# COMPLETE
  V011,
  V012,
  OrcFormatVersion' #-}

instance FromText OrcFormatVersion where
    parser = (OrcFormatVersion' . mk) <$> takeText

instance ToText OrcFormatVersion where
    toText (OrcFormatVersion' ci) = original ci

-- | Represents an enum of /known/ $OrcFormatVersion.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum OrcFormatVersion where
    toEnum i = case i of
        0 -> V011
        1 -> V012
        _ -> (error . showText) $ "Unknown index for OrcFormatVersion: " <> toText i
    fromEnum x = case x of
        V011 -> 0
        V012 -> 1
        OrcFormatVersion' name -> (error . showText) $ "Unknown OrcFormatVersion: " <> original name

-- | Represents the bounds of /known/ $OrcFormatVersion.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded OrcFormatVersion where
    minBound = V011
    maxBound = V012

instance Hashable     OrcFormatVersion
instance NFData       OrcFormatVersion
instance ToByteString OrcFormatVersion
instance ToQuery      OrcFormatVersion
instance ToHeader     OrcFormatVersion

instance ToJSON OrcFormatVersion where
    toJSON = toJSONText

instance FromJSON OrcFormatVersion where
    parseJSON = parseJSONText "OrcFormatVersion"
