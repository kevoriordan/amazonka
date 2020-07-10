{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.ReplicationRunType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SMS.Types.ReplicationRunType (
  ReplicationRunType (
    ..
    , Automatic
    , OnDemand
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ReplicationRunType = ReplicationRunType' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern Automatic :: ReplicationRunType
pattern Automatic = ReplicationRunType' "AUTOMATIC"

pattern OnDemand :: ReplicationRunType
pattern OnDemand = ReplicationRunType' "ON_DEMAND"

{-# COMPLETE
  Automatic,
  OnDemand,
  ReplicationRunType' #-}

instance FromText ReplicationRunType where
    parser = (ReplicationRunType' . mk) <$> takeText

instance ToText ReplicationRunType where
    toText (ReplicationRunType' ci) = original ci

-- | Represents an enum of /known/ $ReplicationRunType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ReplicationRunType where
    toEnum i = case i of
        0 -> Automatic
        1 -> OnDemand
        _ -> (error . showText) $ "Unknown index for ReplicationRunType: " <> toText i
    fromEnum x = case x of
        Automatic -> 0
        OnDemand -> 1
        ReplicationRunType' name -> (error . showText) $ "Unknown ReplicationRunType: " <> original name

-- | Represents the bounds of /known/ $ReplicationRunType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ReplicationRunType where
    minBound = Automatic
    maxBound = OnDemand

instance Hashable     ReplicationRunType
instance NFData       ReplicationRunType
instance ToByteString ReplicationRunType
instance ToQuery      ReplicationRunType
instance ToHeader     ReplicationRunType

instance FromJSON ReplicationRunType where
    parseJSON = parseJSONText "ReplicationRunType"
