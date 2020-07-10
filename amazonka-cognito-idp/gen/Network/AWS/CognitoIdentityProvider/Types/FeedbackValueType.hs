{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.FeedbackValueType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentityProvider.Types.FeedbackValueType (
  FeedbackValueType (
    ..
    , Invalid
    , Valid
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data FeedbackValueType = FeedbackValueType' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern Invalid :: FeedbackValueType
pattern Invalid = FeedbackValueType' "Invalid"

pattern Valid :: FeedbackValueType
pattern Valid = FeedbackValueType' "Valid"

{-# COMPLETE
  Invalid,
  Valid,
  FeedbackValueType' #-}

instance FromText FeedbackValueType where
    parser = (FeedbackValueType' . mk) <$> takeText

instance ToText FeedbackValueType where
    toText (FeedbackValueType' ci) = original ci

-- | Represents an enum of /known/ $FeedbackValueType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum FeedbackValueType where
    toEnum i = case i of
        0 -> Invalid
        1 -> Valid
        _ -> (error . showText) $ "Unknown index for FeedbackValueType: " <> toText i
    fromEnum x = case x of
        Invalid -> 0
        Valid -> 1
        FeedbackValueType' name -> (error . showText) $ "Unknown FeedbackValueType: " <> original name

-- | Represents the bounds of /known/ $FeedbackValueType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded FeedbackValueType where
    minBound = Invalid
    maxBound = Valid

instance Hashable     FeedbackValueType
instance NFData       FeedbackValueType
instance ToByteString FeedbackValueType
instance ToQuery      FeedbackValueType
instance ToHeader     FeedbackValueType

instance ToJSON FeedbackValueType where
    toJSON = toJSONText

instance FromJSON FeedbackValueType where
    parseJSON = parseJSONText "FeedbackValueType"
