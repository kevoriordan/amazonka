{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SubnetState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.SubnetState (
  SubnetState (
    ..
    , SubAvailable
    , SubPending
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data SubnetState = SubnetState' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern SubAvailable :: SubnetState
pattern SubAvailable = SubnetState' "available"

pattern SubPending :: SubnetState
pattern SubPending = SubnetState' "pending"

{-# COMPLETE
  SubAvailable,
  SubPending,
  SubnetState' #-}

instance FromText SubnetState where
    parser = (SubnetState' . mk) <$> takeText

instance ToText SubnetState where
    toText (SubnetState' ci) = original ci

-- | Represents an enum of /known/ $SubnetState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum SubnetState where
    toEnum i = case i of
        0 -> SubAvailable
        1 -> SubPending
        _ -> (error . showText) $ "Unknown index for SubnetState: " <> toText i
    fromEnum x = case x of
        SubAvailable -> 0
        SubPending -> 1
        SubnetState' name -> (error . showText) $ "Unknown SubnetState: " <> original name

-- | Represents the bounds of /known/ $SubnetState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded SubnetState where
    minBound = SubAvailable
    maxBound = SubPending

instance Hashable     SubnetState
instance NFData       SubnetState
instance ToByteString SubnetState
instance ToQuery      SubnetState
instance ToHeader     SubnetState

instance FromXML SubnetState where
    parseXML = parseXMLText "SubnetState"
