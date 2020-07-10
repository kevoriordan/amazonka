{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceLifecycleType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.InstanceLifecycleType (
  InstanceLifecycleType (
    ..
    , ILTScheduled
    , ILTSpot
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data InstanceLifecycleType = InstanceLifecycleType' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern ILTScheduled :: InstanceLifecycleType
pattern ILTScheduled = InstanceLifecycleType' "scheduled"

pattern ILTSpot :: InstanceLifecycleType
pattern ILTSpot = InstanceLifecycleType' "spot"

{-# COMPLETE
  ILTScheduled,
  ILTSpot,
  InstanceLifecycleType' #-}

instance FromText InstanceLifecycleType where
    parser = (InstanceLifecycleType' . mk) <$> takeText

instance ToText InstanceLifecycleType where
    toText (InstanceLifecycleType' ci) = original ci

-- | Represents an enum of /known/ $InstanceLifecycleType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum InstanceLifecycleType where
    toEnum i = case i of
        0 -> ILTScheduled
        1 -> ILTSpot
        _ -> (error . showText) $ "Unknown index for InstanceLifecycleType: " <> toText i
    fromEnum x = case x of
        ILTScheduled -> 0
        ILTSpot -> 1
        InstanceLifecycleType' name -> (error . showText) $ "Unknown InstanceLifecycleType: " <> original name

-- | Represents the bounds of /known/ $InstanceLifecycleType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded InstanceLifecycleType where
    minBound = ILTScheduled
    maxBound = ILTSpot

instance Hashable     InstanceLifecycleType
instance NFData       InstanceLifecycleType
instance ToByteString InstanceLifecycleType
instance ToQuery      InstanceLifecycleType
instance ToHeader     InstanceLifecycleType

instance FromXML InstanceLifecycleType where
    parseXML = parseXMLText "InstanceLifecycleType"
