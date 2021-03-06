{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.SubscriptionType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkDocs.Types.SubscriptionType (
  SubscriptionType (
    ..
    , STAll
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data SubscriptionType = SubscriptionType' (CI Text)
                          deriving (Eq, Ord, Read, Show, Data, Typeable,
                                    Generic)

pattern STAll :: SubscriptionType
pattern STAll = SubscriptionType' "ALL"

{-# COMPLETE
  STAll,
  SubscriptionType' #-}

instance FromText SubscriptionType where
    parser = (SubscriptionType' . mk) <$> takeText

instance ToText SubscriptionType where
    toText (SubscriptionType' ci) = original ci

-- | Represents an enum of /known/ $SubscriptionType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum SubscriptionType where
    toEnum i = case i of
        0 -> STAll
        _ -> (error . showText) $ "Unknown index for SubscriptionType: " <> toText i
    fromEnum x = case x of
        STAll -> 0
        SubscriptionType' name -> (error . showText) $ "Unknown SubscriptionType: " <> original name

-- | Represents the bounds of /known/ $SubscriptionType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded SubscriptionType where
    minBound = STAll
    maxBound = STAll

instance Hashable     SubscriptionType
instance NFData       SubscriptionType
instance ToByteString SubscriptionType
instance ToQuery      SubscriptionType
instance ToHeader     SubscriptionType

instance ToJSON SubscriptionType where
    toJSON = toJSONText
