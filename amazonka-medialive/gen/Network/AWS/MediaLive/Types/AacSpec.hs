{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AacSpec
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.AacSpec (
  AacSpec (
    ..
    , ASMPEG2
    , ASMPEG4
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Aac Spec
data AacSpec = AacSpec' (CI Text)
                 deriving (Eq, Ord, Read, Show, Data, Typeable,
                           Generic)

pattern ASMPEG2 :: AacSpec
pattern ASMPEG2 = AacSpec' "MPEG2"

pattern ASMPEG4 :: AacSpec
pattern ASMPEG4 = AacSpec' "MPEG4"

{-# COMPLETE
  ASMPEG2,
  ASMPEG4,
  AacSpec' #-}

instance FromText AacSpec where
    parser = (AacSpec' . mk) <$> takeText

instance ToText AacSpec where
    toText (AacSpec' ci) = original ci

-- | Represents an enum of /known/ $AacSpec.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AacSpec where
    toEnum i = case i of
        0 -> ASMPEG2
        1 -> ASMPEG4
        _ -> (error . showText) $ "Unknown index for AacSpec: " <> toText i
    fromEnum x = case x of
        ASMPEG2 -> 0
        ASMPEG4 -> 1
        AacSpec' name -> (error . showText) $ "Unknown AacSpec: " <> original name

-- | Represents the bounds of /known/ $AacSpec.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AacSpec where
    minBound = ASMPEG2
    maxBound = ASMPEG4

instance Hashable     AacSpec
instance NFData       AacSpec
instance ToByteString AacSpec
instance ToQuery      AacSpec
instance ToHeader     AacSpec

instance ToJSON AacSpec where
    toJSON = toJSONText

instance FromJSON AacSpec where
    parseJSON = parseJSONText "AacSpec"
