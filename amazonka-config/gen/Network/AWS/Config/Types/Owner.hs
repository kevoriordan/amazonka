{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.Owner
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Config.Types.Owner (
  Owner (
    ..
    , AWS
    , CustomLambda
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data Owner = Owner' (CI Text)
               deriving (Eq, Ord, Read, Show, Data, Typeable,
                         Generic)

pattern AWS :: Owner
pattern AWS = Owner' "AWS"

pattern CustomLambda :: Owner
pattern CustomLambda = Owner' "CUSTOM_LAMBDA"

{-# COMPLETE
  AWS,
  CustomLambda,
  Owner' #-}

instance FromText Owner where
    parser = (Owner' . mk) <$> takeText

instance ToText Owner where
    toText (Owner' ci) = original ci

-- | Represents an enum of /known/ $Owner.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Owner where
    toEnum i = case i of
        0 -> AWS
        1 -> CustomLambda
        _ -> (error . showText) $ "Unknown index for Owner: " <> toText i
    fromEnum x = case x of
        AWS -> 0
        CustomLambda -> 1
        Owner' name -> (error . showText) $ "Unknown Owner: " <> original name

-- | Represents the bounds of /known/ $Owner.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Owner where
    minBound = AWS
    maxBound = CustomLambda

instance Hashable     Owner
instance NFData       Owner
instance ToByteString Owner
instance ToQuery      Owner
instance ToHeader     Owner

instance ToJSON Owner where
    toJSON = toJSONText

instance FromJSON Owner where
    parseJSON = parseJSONText "Owner"
