{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.AttackLayer
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Shield.Types.AttackLayer (
  AttackLayer (
    ..
    , Application
    , Network
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AttackLayer = AttackLayer' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern Application :: AttackLayer
pattern Application = AttackLayer' "APPLICATION"

pattern Network :: AttackLayer
pattern Network = AttackLayer' "NETWORK"

{-# COMPLETE
  Application,
  Network,
  AttackLayer' #-}

instance FromText AttackLayer where
    parser = (AttackLayer' . mk) <$> takeText

instance ToText AttackLayer where
    toText (AttackLayer' ci) = original ci

-- | Represents an enum of /known/ $AttackLayer.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AttackLayer where
    toEnum i = case i of
        0 -> Application
        1 -> Network
        _ -> (error . showText) $ "Unknown index for AttackLayer: " <> toText i
    fromEnum x = case x of
        Application -> 0
        Network -> 1
        AttackLayer' name -> (error . showText) $ "Unknown AttackLayer: " <> original name

-- | Represents the bounds of /known/ $AttackLayer.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AttackLayer where
    minBound = Application
    maxBound = Network

instance Hashable     AttackLayer
instance NFData       AttackLayer
instance ToByteString AttackLayer
instance ToQuery      AttackLayer
instance ToHeader     AttackLayer

instance FromJSON AttackLayer where
    parseJSON = parseJSONText "AttackLayer"
