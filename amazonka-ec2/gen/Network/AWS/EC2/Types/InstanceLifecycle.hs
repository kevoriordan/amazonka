{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceLifecycle
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.InstanceLifecycle (
  InstanceLifecycle (
    ..
    , ILOnDemand
    , ILSpot
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data InstanceLifecycle = InstanceLifecycle' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern ILOnDemand :: InstanceLifecycle
pattern ILOnDemand = InstanceLifecycle' "on-demand"

pattern ILSpot :: InstanceLifecycle
pattern ILSpot = InstanceLifecycle' "spot"

{-# COMPLETE
  ILOnDemand,
  ILSpot,
  InstanceLifecycle' #-}

instance FromText InstanceLifecycle where
    parser = (InstanceLifecycle' . mk) <$> takeText

instance ToText InstanceLifecycle where
    toText (InstanceLifecycle' ci) = original ci

-- | Represents an enum of /known/ $InstanceLifecycle.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum InstanceLifecycle where
    toEnum i = case i of
        0 -> ILOnDemand
        1 -> ILSpot
        _ -> (error . showText) $ "Unknown index for InstanceLifecycle: " <> toText i
    fromEnum x = case x of
        ILOnDemand -> 0
        ILSpot -> 1
        InstanceLifecycle' name -> (error . showText) $ "Unknown InstanceLifecycle: " <> original name

-- | Represents the bounds of /known/ $InstanceLifecycle.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded InstanceLifecycle where
    minBound = ILOnDemand
    maxBound = ILSpot

instance Hashable     InstanceLifecycle
instance NFData       InstanceLifecycle
instance ToByteString InstanceLifecycle
instance ToQuery      InstanceLifecycle
instance ToHeader     InstanceLifecycle

instance FromXML InstanceLifecycle where
    parseXML = parseXMLText "InstanceLifecycle"
