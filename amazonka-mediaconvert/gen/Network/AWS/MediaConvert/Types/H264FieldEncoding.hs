{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H264FieldEncoding
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.H264FieldEncoding (
  H264FieldEncoding (
    ..
    , ForceField
    , Paff
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Choosing FORCE_FIELD disables PAFF encoding for interlaced outputs.
data H264FieldEncoding = H264FieldEncoding' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern ForceField :: H264FieldEncoding
pattern ForceField = H264FieldEncoding' "FORCE_FIELD"

pattern Paff :: H264FieldEncoding
pattern Paff = H264FieldEncoding' "PAFF"

{-# COMPLETE
  ForceField,
  Paff,
  H264FieldEncoding' #-}

instance FromText H264FieldEncoding where
    parser = (H264FieldEncoding' . mk) <$> takeText

instance ToText H264FieldEncoding where
    toText (H264FieldEncoding' ci) = original ci

-- | Represents an enum of /known/ $H264FieldEncoding.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum H264FieldEncoding where
    toEnum i = case i of
        0 -> ForceField
        1 -> Paff
        _ -> (error . showText) $ "Unknown index for H264FieldEncoding: " <> toText i
    fromEnum x = case x of
        ForceField -> 0
        Paff -> 1
        H264FieldEncoding' name -> (error . showText) $ "Unknown H264FieldEncoding: " <> original name

-- | Represents the bounds of /known/ $H264FieldEncoding.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded H264FieldEncoding where
    minBound = ForceField
    maxBound = Paff

instance Hashable     H264FieldEncoding
instance NFData       H264FieldEncoding
instance ToByteString H264FieldEncoding
instance ToQuery      H264FieldEncoding
instance ToHeader     H264FieldEncoding

instance ToJSON H264FieldEncoding where
    toJSON = toJSONText

instance FromJSON H264FieldEncoding where
    parseJSON = parseJSONText "H264FieldEncoding"
