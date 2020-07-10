{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ConversionTaskState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ConversionTaskState (
  ConversionTaskState (
    ..
    , CTSActive
    , CTSCancelled
    , CTSCancelling
    , CTSCompleted
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data ConversionTaskState = ConversionTaskState' (CI
                                                   Text)
                             deriving (Eq, Ord, Read, Show, Data, Typeable,
                                       Generic)

pattern CTSActive :: ConversionTaskState
pattern CTSActive = ConversionTaskState' "active"

pattern CTSCancelled :: ConversionTaskState
pattern CTSCancelled = ConversionTaskState' "cancelled"

pattern CTSCancelling :: ConversionTaskState
pattern CTSCancelling = ConversionTaskState' "cancelling"

pattern CTSCompleted :: ConversionTaskState
pattern CTSCompleted = ConversionTaskState' "completed"

{-# COMPLETE
  CTSActive,
  CTSCancelled,
  CTSCancelling,
  CTSCompleted,
  ConversionTaskState' #-}

instance FromText ConversionTaskState where
    parser = (ConversionTaskState' . mk) <$> takeText

instance ToText ConversionTaskState where
    toText (ConversionTaskState' ci) = original ci

-- | Represents an enum of /known/ $ConversionTaskState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ConversionTaskState where
    toEnum i = case i of
        0 -> CTSActive
        1 -> CTSCancelled
        2 -> CTSCancelling
        3 -> CTSCompleted
        _ -> (error . showText) $ "Unknown index for ConversionTaskState: " <> toText i
    fromEnum x = case x of
        CTSActive -> 0
        CTSCancelled -> 1
        CTSCancelling -> 2
        CTSCompleted -> 3
        ConversionTaskState' name -> (error . showText) $ "Unknown ConversionTaskState: " <> original name

-- | Represents the bounds of /known/ $ConversionTaskState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ConversionTaskState where
    minBound = CTSActive
    maxBound = CTSCompleted

instance Hashable     ConversionTaskState
instance NFData       ConversionTaskState
instance ToByteString ConversionTaskState
instance ToQuery      ConversionTaskState
instance ToHeader     ConversionTaskState

instance FromXML ConversionTaskState where
    parseXML = parseXMLText "ConversionTaskState"
