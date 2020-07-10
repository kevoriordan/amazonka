{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.EC2TagFilterType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeDeploy.Types.EC2TagFilterType (
  EC2TagFilterType (
    ..
    , KeyAndValue
    , KeyOnly
    , ValueOnly
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data EC2TagFilterType = EC2TagFilterType' (CI Text)
                          deriving (Eq, Ord, Read, Show, Data, Typeable,
                                    Generic)

pattern KeyAndValue :: EC2TagFilterType
pattern KeyAndValue = EC2TagFilterType' "KEY_AND_VALUE"

pattern KeyOnly :: EC2TagFilterType
pattern KeyOnly = EC2TagFilterType' "KEY_ONLY"

pattern ValueOnly :: EC2TagFilterType
pattern ValueOnly = EC2TagFilterType' "VALUE_ONLY"

{-# COMPLETE
  KeyAndValue,
  KeyOnly,
  ValueOnly,
  EC2TagFilterType' #-}

instance FromText EC2TagFilterType where
    parser = (EC2TagFilterType' . mk) <$> takeText

instance ToText EC2TagFilterType where
    toText (EC2TagFilterType' ci) = original ci

-- | Represents an enum of /known/ $EC2TagFilterType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum EC2TagFilterType where
    toEnum i = case i of
        0 -> KeyAndValue
        1 -> KeyOnly
        2 -> ValueOnly
        _ -> (error . showText) $ "Unknown index for EC2TagFilterType: " <> toText i
    fromEnum x = case x of
        KeyAndValue -> 0
        KeyOnly -> 1
        ValueOnly -> 2
        EC2TagFilterType' name -> (error . showText) $ "Unknown EC2TagFilterType: " <> original name

-- | Represents the bounds of /known/ $EC2TagFilterType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded EC2TagFilterType where
    minBound = KeyAndValue
    maxBound = ValueOnly

instance Hashable     EC2TagFilterType
instance NFData       EC2TagFilterType
instance ToByteString EC2TagFilterType
instance ToQuery      EC2TagFilterType
instance ToHeader     EC2TagFilterType

instance ToJSON EC2TagFilterType where
    toJSON = toJSONText

instance FromJSON EC2TagFilterType where
    parseJSON = parseJSONText "EC2TagFilterType"
