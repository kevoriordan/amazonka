{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.Types.ThroughputMode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EFS.Types.ThroughputMode (
  ThroughputMode (
    ..
    , Bursting
    , Provisioned
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ThroughputMode = ThroughputMode' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern Bursting :: ThroughputMode
pattern Bursting = ThroughputMode' "bursting"

pattern Provisioned :: ThroughputMode
pattern Provisioned = ThroughputMode' "provisioned"

{-# COMPLETE
  Bursting,
  Provisioned,
  ThroughputMode' #-}

instance FromText ThroughputMode where
    parser = (ThroughputMode' . mk) <$> takeText

instance ToText ThroughputMode where
    toText (ThroughputMode' ci) = original ci

-- | Represents an enum of /known/ $ThroughputMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ThroughputMode where
    toEnum i = case i of
        0 -> Bursting
        1 -> Provisioned
        _ -> (error . showText) $ "Unknown index for ThroughputMode: " <> toText i
    fromEnum x = case x of
        Bursting -> 0
        Provisioned -> 1
        ThroughputMode' name -> (error . showText) $ "Unknown ThroughputMode: " <> original name

-- | Represents the bounds of /known/ $ThroughputMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ThroughputMode where
    minBound = Bursting
    maxBound = Provisioned

instance Hashable     ThroughputMode
instance NFData       ThroughputMode
instance ToByteString ThroughputMode
instance ToQuery      ThroughputMode
instance ToHeader     ThroughputMode

instance ToJSON ThroughputMode where
    toJSON = toJSONText

instance FromJSON ThroughputMode where
    parseJSON = parseJSONText "ThroughputMode"
