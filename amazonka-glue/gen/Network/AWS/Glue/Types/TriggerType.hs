{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.TriggerType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.TriggerType (
  TriggerType (
    ..
    , Conditional
    , OnDemand
    , Scheduled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data TriggerType = TriggerType' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern Conditional :: TriggerType
pattern Conditional = TriggerType' "CONDITIONAL"

pattern OnDemand :: TriggerType
pattern OnDemand = TriggerType' "ON_DEMAND"

pattern Scheduled :: TriggerType
pattern Scheduled = TriggerType' "SCHEDULED"

{-# COMPLETE
  Conditional,
  OnDemand,
  Scheduled,
  TriggerType' #-}

instance FromText TriggerType where
    parser = (TriggerType' . mk) <$> takeText

instance ToText TriggerType where
    toText (TriggerType' ci) = original ci

-- | Represents an enum of /known/ $TriggerType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum TriggerType where
    toEnum i = case i of
        0 -> Conditional
        1 -> OnDemand
        2 -> Scheduled
        _ -> (error . showText) $ "Unknown index for TriggerType: " <> toText i
    fromEnum x = case x of
        Conditional -> 0
        OnDemand -> 1
        Scheduled -> 2
        TriggerType' name -> (error . showText) $ "Unknown TriggerType: " <> original name

-- | Represents the bounds of /known/ $TriggerType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded TriggerType where
    minBound = Conditional
    maxBound = Scheduled

instance Hashable     TriggerType
instance NFData       TriggerType
instance ToByteString TriggerType
instance ToQuery      TriggerType
instance ToHeader     TriggerType

instance ToJSON TriggerType where
    toJSON = toJSONText

instance FromJSON TriggerType where
    parseJSON = parseJSONText "TriggerType"
