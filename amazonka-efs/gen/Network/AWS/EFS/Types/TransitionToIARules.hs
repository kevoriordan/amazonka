{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.Types.TransitionToIARules
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EFS.Types.TransitionToIARules (
  TransitionToIARules (
    ..
    , After14Days
    , After30Days
    , After60Days
    , After7Days
    , After90Days
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data TransitionToIARules = TransitionToIARules' (CI
                                                   Text)
                             deriving (Eq, Ord, Read, Show, Data, Typeable,
                                       Generic)

pattern After14Days :: TransitionToIARules
pattern After14Days = TransitionToIARules' "AFTER_14_DAYS"

pattern After30Days :: TransitionToIARules
pattern After30Days = TransitionToIARules' "AFTER_30_DAYS"

pattern After60Days :: TransitionToIARules
pattern After60Days = TransitionToIARules' "AFTER_60_DAYS"

pattern After7Days :: TransitionToIARules
pattern After7Days = TransitionToIARules' "AFTER_7_DAYS"

pattern After90Days :: TransitionToIARules
pattern After90Days = TransitionToIARules' "AFTER_90_DAYS"

{-# COMPLETE
  After14Days,
  After30Days,
  After60Days,
  After7Days,
  After90Days,
  TransitionToIARules' #-}

instance FromText TransitionToIARules where
    parser = (TransitionToIARules' . mk) <$> takeText

instance ToText TransitionToIARules where
    toText (TransitionToIARules' ci) = original ci

-- | Represents an enum of /known/ $TransitionToIARules.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum TransitionToIARules where
    toEnum i = case i of
        0 -> After14Days
        1 -> After30Days
        2 -> After60Days
        3 -> After7Days
        4 -> After90Days
        _ -> (error . showText) $ "Unknown index for TransitionToIARules: " <> toText i
    fromEnum x = case x of
        After14Days -> 0
        After30Days -> 1
        After60Days -> 2
        After7Days -> 3
        After90Days -> 4
        TransitionToIARules' name -> (error . showText) $ "Unknown TransitionToIARules: " <> original name

-- | Represents the bounds of /known/ $TransitionToIARules.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded TransitionToIARules where
    minBound = After14Days
    maxBound = After90Days

instance Hashable     TransitionToIARules
instance NFData       TransitionToIARules
instance ToByteString TransitionToIARules
instance ToQuery      TransitionToIARules
instance ToHeader     TransitionToIARules

instance ToJSON TransitionToIARules where
    toJSON = toJSONText

instance FromJSON TransitionToIARules where
    parseJSON = parseJSONText "TransitionToIARules"
