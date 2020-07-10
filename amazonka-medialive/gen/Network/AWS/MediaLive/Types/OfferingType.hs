{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.OfferingType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.OfferingType (
  OfferingType (
    ..
    , NoUpfront
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Offering type, e.g. 'NO_UPFRONT'
data OfferingType = OfferingType' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern NoUpfront :: OfferingType
pattern NoUpfront = OfferingType' "NO_UPFRONT"

{-# COMPLETE
  NoUpfront,
  OfferingType' #-}

instance FromText OfferingType where
    parser = (OfferingType' . mk) <$> takeText

instance ToText OfferingType where
    toText (OfferingType' ci) = original ci

-- | Represents an enum of /known/ $OfferingType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum OfferingType where
    toEnum i = case i of
        0 -> NoUpfront
        _ -> (error . showText) $ "Unknown index for OfferingType: " <> toText i
    fromEnum x = case x of
        NoUpfront -> 0
        OfferingType' name -> (error . showText) $ "Unknown OfferingType: " <> original name

-- | Represents the bounds of /known/ $OfferingType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded OfferingType where
    minBound = NoUpfront
    maxBound = NoUpfront

instance Hashable     OfferingType
instance NFData       OfferingType
instance ToByteString OfferingType
instance ToQuery      OfferingType
instance ToHeader     OfferingType

instance FromJSON OfferingType where
    parseJSON = parseJSONText "OfferingType"
