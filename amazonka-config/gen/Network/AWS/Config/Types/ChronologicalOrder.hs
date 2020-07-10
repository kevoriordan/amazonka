{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ChronologicalOrder
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Config.Types.ChronologicalOrder (
  ChronologicalOrder (
    ..
    , Forward
    , Reverse
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ChronologicalOrder = ChronologicalOrder' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern Forward :: ChronologicalOrder
pattern Forward = ChronologicalOrder' "Forward"

pattern Reverse :: ChronologicalOrder
pattern Reverse = ChronologicalOrder' "Reverse"

{-# COMPLETE
  Forward,
  Reverse,
  ChronologicalOrder' #-}

instance FromText ChronologicalOrder where
    parser = (ChronologicalOrder' . mk) <$> takeText

instance ToText ChronologicalOrder where
    toText (ChronologicalOrder' ci) = original ci

-- | Represents an enum of /known/ $ChronologicalOrder.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ChronologicalOrder where
    toEnum i = case i of
        0 -> Forward
        1 -> Reverse
        _ -> (error . showText) $ "Unknown index for ChronologicalOrder: " <> toText i
    fromEnum x = case x of
        Forward -> 0
        Reverse -> 1
        ChronologicalOrder' name -> (error . showText) $ "Unknown ChronologicalOrder: " <> original name

-- | Represents the bounds of /known/ $ChronologicalOrder.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ChronologicalOrder where
    minBound = Forward
    maxBound = Reverse

instance Hashable     ChronologicalOrder
instance NFData       ChronologicalOrder
instance ToByteString ChronologicalOrder
instance ToQuery      ChronologicalOrder
instance ToHeader     ChronologicalOrder

instance ToJSON ChronologicalOrder where
    toJSON = toJSONText
