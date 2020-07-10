{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.AttributeType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.AttributeType (
  AttributeType (
    ..
    , Exclusive
    , Inclusive
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AttributeType = AttributeType' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern Exclusive :: AttributeType
pattern Exclusive = AttributeType' "EXCLUSIVE"

pattern Inclusive :: AttributeType
pattern Inclusive = AttributeType' "INCLUSIVE"

{-# COMPLETE
  Exclusive,
  Inclusive,
  AttributeType' #-}

instance FromText AttributeType where
    parser = (AttributeType' . mk) <$> takeText

instance ToText AttributeType where
    toText (AttributeType' ci) = original ci

-- | Represents an enum of /known/ $AttributeType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AttributeType where
    toEnum i = case i of
        0 -> Exclusive
        1 -> Inclusive
        _ -> (error . showText) $ "Unknown index for AttributeType: " <> toText i
    fromEnum x = case x of
        Exclusive -> 0
        Inclusive -> 1
        AttributeType' name -> (error . showText) $ "Unknown AttributeType: " <> original name

-- | Represents the bounds of /known/ $AttributeType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AttributeType where
    minBound = Exclusive
    maxBound = Inclusive

instance Hashable     AttributeType
instance NFData       AttributeType
instance ToByteString AttributeType
instance ToQuery      AttributeType
instance ToHeader     AttributeType

instance ToJSON AttributeType where
    toJSON = toJSONText

instance FromJSON AttributeType where
    parseJSON = parseJSONText "AttributeType"
