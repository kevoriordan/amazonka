{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Polly.Types.Gender
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Polly.Types.Gender (
  Gender (
    ..
    , Female
    , Male
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data Gender = Gender' (CI Text)
                deriving (Eq, Ord, Read, Show, Data, Typeable,
                          Generic)

pattern Female :: Gender
pattern Female = Gender' "Female"

pattern Male :: Gender
pattern Male = Gender' "Male"

{-# COMPLETE
  Female,
  Male,
  Gender' #-}

instance FromText Gender where
    parser = (Gender' . mk) <$> takeText

instance ToText Gender where
    toText (Gender' ci) = original ci

-- | Represents an enum of /known/ $Gender.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Gender where
    toEnum i = case i of
        0 -> Female
        1 -> Male
        _ -> (error . showText) $ "Unknown index for Gender: " <> toText i
    fromEnum x = case x of
        Female -> 0
        Male -> 1
        Gender' name -> (error . showText) $ "Unknown Gender: " <> original name

-- | Represents the bounds of /known/ $Gender.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Gender where
    minBound = Female
    maxBound = Male

instance Hashable     Gender
instance NFData       Gender
instance ToByteString Gender
instance ToQuery      Gender
instance ToHeader     Gender

instance FromJSON Gender where
    parseJSON = parseJSONText "Gender"
