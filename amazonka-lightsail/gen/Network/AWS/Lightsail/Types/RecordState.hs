{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.RecordState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.RecordState (
  RecordState (
    ..
    , RSFailed
    , RSStarted
    , RSSucceeded
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data RecordState = RecordState' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern RSFailed :: RecordState
pattern RSFailed = RecordState' "Failed"

pattern RSStarted :: RecordState
pattern RSStarted = RecordState' "Started"

pattern RSSucceeded :: RecordState
pattern RSSucceeded = RecordState' "Succeeded"

{-# COMPLETE
  RSFailed,
  RSStarted,
  RSSucceeded,
  RecordState' #-}

instance FromText RecordState where
    parser = (RecordState' . mk) <$> takeText

instance ToText RecordState where
    toText (RecordState' ci) = original ci

-- | Represents an enum of /known/ $RecordState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum RecordState where
    toEnum i = case i of
        0 -> RSFailed
        1 -> RSStarted
        2 -> RSSucceeded
        _ -> (error . showText) $ "Unknown index for RecordState: " <> toText i
    fromEnum x = case x of
        RSFailed -> 0
        RSStarted -> 1
        RSSucceeded -> 2
        RecordState' name -> (error . showText) $ "Unknown RecordState: " <> original name

-- | Represents the bounds of /known/ $RecordState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded RecordState where
    minBound = RSFailed
    maxBound = RSSucceeded

instance Hashable     RecordState
instance NFData       RecordState
instance ToByteString RecordState
instance ToQuery      RecordState
instance ToHeader     RecordState

instance FromJSON RecordState where
    parseJSON = parseJSONText "RecordState"
