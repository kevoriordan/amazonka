{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AacRawFormat
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.AacRawFormat (
  AacRawFormat (
    ..
    , ARFLatmLoas
    , ARFNone
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Enables LATM/LOAS AAC output. Note that if you use LATM/LOAS AAC in an output, you must choose "No container" for the output container.
data AacRawFormat = AacRawFormat' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern ARFLatmLoas :: AacRawFormat
pattern ARFLatmLoas = AacRawFormat' "LATM_LOAS"

pattern ARFNone :: AacRawFormat
pattern ARFNone = AacRawFormat' "NONE"

{-# COMPLETE
  ARFLatmLoas,
  ARFNone,
  AacRawFormat' #-}

instance FromText AacRawFormat where
    parser = (AacRawFormat' . mk) <$> takeText

instance ToText AacRawFormat where
    toText (AacRawFormat' ci) = original ci

-- | Represents an enum of /known/ $AacRawFormat.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AacRawFormat where
    toEnum i = case i of
        0 -> ARFLatmLoas
        1 -> ARFNone
        _ -> (error . showText) $ "Unknown index for AacRawFormat: " <> toText i
    fromEnum x = case x of
        ARFLatmLoas -> 0
        ARFNone -> 1
        AacRawFormat' name -> (error . showText) $ "Unknown AacRawFormat: " <> original name

-- | Represents the bounds of /known/ $AacRawFormat.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AacRawFormat where
    minBound = ARFLatmLoas
    maxBound = ARFNone

instance Hashable     AacRawFormat
instance NFData       AacRawFormat
instance ToByteString AacRawFormat
instance ToQuery      AacRawFormat
instance ToHeader     AacRawFormat

instance ToJSON AacRawFormat where
    toJSON = toJSONText

instance FromJSON AacRawFormat where
    parseJSON = parseJSONText "AacRawFormat"
