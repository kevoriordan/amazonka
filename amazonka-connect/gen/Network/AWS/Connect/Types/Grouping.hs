{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.Grouping
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Connect.Types.Grouping (
  Grouping (
    ..
    , Channel
    , Queue
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data Grouping = Grouping' (CI Text)
                  deriving (Eq, Ord, Read, Show, Data, Typeable,
                            Generic)

pattern Channel :: Grouping
pattern Channel = Grouping' "CHANNEL"

pattern Queue :: Grouping
pattern Queue = Grouping' "QUEUE"

{-# COMPLETE
  Channel,
  Queue,
  Grouping' #-}

instance FromText Grouping where
    parser = (Grouping' . mk) <$> takeText

instance ToText Grouping where
    toText (Grouping' ci) = original ci

-- | Represents an enum of /known/ $Grouping.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Grouping where
    toEnum i = case i of
        0 -> Channel
        1 -> Queue
        _ -> (error . showText) $ "Unknown index for Grouping: " <> toText i
    fromEnum x = case x of
        Channel -> 0
        Queue -> 1
        Grouping' name -> (error . showText) $ "Unknown Grouping: " <> original name

-- | Represents the bounds of /known/ $Grouping.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Grouping where
    minBound = Channel
    maxBound = Queue

instance Hashable     Grouping
instance NFData       Grouping
instance ToByteString Grouping
instance ToQuery      Grouping
instance ToHeader     Grouping

instance ToJSON Grouping where
    toJSON = toJSONText
