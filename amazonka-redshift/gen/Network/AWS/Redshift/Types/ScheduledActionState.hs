{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.ScheduledActionState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Redshift.Types.ScheduledActionState (
  ScheduledActionState (
    ..
    , SASActive
    , SASDisabled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude
import Network.AWS.Redshift.Internal

data ScheduledActionState = ScheduledActionState' (CI
                                                     Text)
                              deriving (Eq, Ord, Read, Show, Data, Typeable,
                                        Generic)

pattern SASActive :: ScheduledActionState
pattern SASActive = ScheduledActionState' "ACTIVE"

pattern SASDisabled :: ScheduledActionState
pattern SASDisabled = ScheduledActionState' "DISABLED"

{-# COMPLETE
  SASActive,
  SASDisabled,
  ScheduledActionState' #-}

instance FromText ScheduledActionState where
    parser = (ScheduledActionState' . mk) <$> takeText

instance ToText ScheduledActionState where
    toText (ScheduledActionState' ci) = original ci

-- | Represents an enum of /known/ $ScheduledActionState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ScheduledActionState where
    toEnum i = case i of
        0 -> SASActive
        1 -> SASDisabled
        _ -> (error . showText) $ "Unknown index for ScheduledActionState: " <> toText i
    fromEnum x = case x of
        SASActive -> 0
        SASDisabled -> 1
        ScheduledActionState' name -> (error . showText) $ "Unknown ScheduledActionState: " <> original name

-- | Represents the bounds of /known/ $ScheduledActionState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ScheduledActionState where
    minBound = SASActive
    maxBound = SASDisabled

instance Hashable     ScheduledActionState
instance NFData       ScheduledActionState
instance ToByteString ScheduledActionState
instance ToQuery      ScheduledActionState
instance ToHeader     ScheduledActionState

instance FromXML ScheduledActionState where
    parseXML = parseXMLText "ScheduledActionState"
