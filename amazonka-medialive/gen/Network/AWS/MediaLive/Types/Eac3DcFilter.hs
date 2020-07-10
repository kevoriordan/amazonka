{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Eac3DcFilter
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.Eac3DcFilter (
  Eac3DcFilter (
    ..
    , EDFDisabled
    , EDFEnabled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Eac3 Dc Filter
data Eac3DcFilter = Eac3DcFilter' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern EDFDisabled :: Eac3DcFilter
pattern EDFDisabled = Eac3DcFilter' "DISABLED"

pattern EDFEnabled :: Eac3DcFilter
pattern EDFEnabled = Eac3DcFilter' "ENABLED"

{-# COMPLETE
  EDFDisabled,
  EDFEnabled,
  Eac3DcFilter' #-}

instance FromText Eac3DcFilter where
    parser = (Eac3DcFilter' . mk) <$> takeText

instance ToText Eac3DcFilter where
    toText (Eac3DcFilter' ci) = original ci

-- | Represents an enum of /known/ $Eac3DcFilter.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Eac3DcFilter where
    toEnum i = case i of
        0 -> EDFDisabled
        1 -> EDFEnabled
        _ -> (error . showText) $ "Unknown index for Eac3DcFilter: " <> toText i
    fromEnum x = case x of
        EDFDisabled -> 0
        EDFEnabled -> 1
        Eac3DcFilter' name -> (error . showText) $ "Unknown Eac3DcFilter: " <> original name

-- | Represents the bounds of /known/ $Eac3DcFilter.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Eac3DcFilter where
    minBound = EDFDisabled
    maxBound = EDFEnabled

instance Hashable     Eac3DcFilter
instance NFData       Eac3DcFilter
instance ToByteString Eac3DcFilter
instance ToQuery      Eac3DcFilter
instance ToHeader     Eac3DcFilter

instance ToJSON Eac3DcFilter where
    toJSON = toJSONText

instance FromJSON Eac3DcFilter where
    parseJSON = parseJSONText "Eac3DcFilter"
