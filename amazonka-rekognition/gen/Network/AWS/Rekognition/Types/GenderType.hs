{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.GenderType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Rekognition.Types.GenderType (
  GenderType (
    ..
    , Female
    , Male
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data GenderType = GenderType' (CI Text)
                    deriving (Eq, Ord, Read, Show, Data, Typeable,
                              Generic)

pattern Female :: GenderType
pattern Female = GenderType' "Female"

pattern Male :: GenderType
pattern Male = GenderType' "Male"

{-# COMPLETE
  Female,
  Male,
  GenderType' #-}

instance FromText GenderType where
    parser = (GenderType' . mk) <$> takeText

instance ToText GenderType where
    toText (GenderType' ci) = original ci

-- | Represents an enum of /known/ $GenderType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum GenderType where
    toEnum i = case i of
        0 -> Female
        1 -> Male
        _ -> (error . showText) $ "Unknown index for GenderType: " <> toText i
    fromEnum x = case x of
        Female -> 0
        Male -> 1
        GenderType' name -> (error . showText) $ "Unknown GenderType: " <> original name

-- | Represents the bounds of /known/ $GenderType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded GenderType where
    minBound = Female
    maxBound = Male

instance Hashable     GenderType
instance NFData       GenderType
instance ToByteString GenderType
instance ToQuery      GenderType
instance ToHeader     GenderType

instance FromJSON GenderType where
    parseJSON = parseJSONText "GenderType"
