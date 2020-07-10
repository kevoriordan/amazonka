{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.Connectivity
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECS.Types.Connectivity (
  Connectivity (
    ..
    , Connected
    , Disconnected
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data Connectivity = Connectivity' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern Connected :: Connectivity
pattern Connected = Connectivity' "CONNECTED"

pattern Disconnected :: Connectivity
pattern Disconnected = Connectivity' "DISCONNECTED"

{-# COMPLETE
  Connected,
  Disconnected,
  Connectivity' #-}

instance FromText Connectivity where
    parser = (Connectivity' . mk) <$> takeText

instance ToText Connectivity where
    toText (Connectivity' ci) = original ci

-- | Represents an enum of /known/ $Connectivity.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Connectivity where
    toEnum i = case i of
        0 -> Connected
        1 -> Disconnected
        _ -> (error . showText) $ "Unknown index for Connectivity: " <> toText i
    fromEnum x = case x of
        Connected -> 0
        Disconnected -> 1
        Connectivity' name -> (error . showText) $ "Unknown Connectivity: " <> original name

-- | Represents the bounds of /known/ $Connectivity.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Connectivity where
    minBound = Connected
    maxBound = Disconnected

instance Hashable     Connectivity
instance NFData       Connectivity
instance ToByteString Connectivity
instance ToQuery      Connectivity
instance ToHeader     Connectivity

instance FromJSON Connectivity where
    parseJSON = parseJSONText "Connectivity"
