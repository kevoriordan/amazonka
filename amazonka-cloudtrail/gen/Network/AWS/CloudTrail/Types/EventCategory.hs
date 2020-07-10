{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.Types.EventCategory
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudTrail.Types.EventCategory (
  EventCategory (
    ..
    , Insight
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data EventCategory = EventCategory' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern Insight :: EventCategory
pattern Insight = EventCategory' "insight"

{-# COMPLETE
  Insight,
  EventCategory' #-}

instance FromText EventCategory where
    parser = (EventCategory' . mk) <$> takeText

instance ToText EventCategory where
    toText (EventCategory' ci) = original ci

-- | Represents an enum of /known/ $EventCategory.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum EventCategory where
    toEnum i = case i of
        0 -> Insight
        _ -> (error . showText) $ "Unknown index for EventCategory: " <> toText i
    fromEnum x = case x of
        Insight -> 0
        EventCategory' name -> (error . showText) $ "Unknown EventCategory: " <> original name

-- | Represents the bounds of /known/ $EventCategory.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded EventCategory where
    minBound = Insight
    maxBound = Insight

instance Hashable     EventCategory
instance NFData       EventCategory
instance ToByteString EventCategory
instance ToQuery      EventCategory
instance ToHeader     EventCategory

instance ToJSON EventCategory where
    toJSON = toJSONText
