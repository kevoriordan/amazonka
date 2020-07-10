{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.CalendarState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.CalendarState (
  CalendarState (
    ..
    , CSClosed
    , CSOpen
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data CalendarState = CalendarState' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern CSClosed :: CalendarState
pattern CSClosed = CalendarState' "CLOSED"

pattern CSOpen :: CalendarState
pattern CSOpen = CalendarState' "OPEN"

{-# COMPLETE
  CSClosed,
  CSOpen,
  CalendarState' #-}

instance FromText CalendarState where
    parser = (CalendarState' . mk) <$> takeText

instance ToText CalendarState where
    toText (CalendarState' ci) = original ci

-- | Represents an enum of /known/ $CalendarState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum CalendarState where
    toEnum i = case i of
        0 -> CSClosed
        1 -> CSOpen
        _ -> (error . showText) $ "Unknown index for CalendarState: " <> toText i
    fromEnum x = case x of
        CSClosed -> 0
        CSOpen -> 1
        CalendarState' name -> (error . showText) $ "Unknown CalendarState: " <> original name

-- | Represents the bounds of /known/ $CalendarState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded CalendarState where
    minBound = CSClosed
    maxBound = CSOpen

instance Hashable     CalendarState
instance NFData       CalendarState
instance ToByteString CalendarState
instance ToQuery      CalendarState
instance ToHeader     CalendarState

instance FromJSON CalendarState where
    parseJSON = parseJSONText "CalendarState"
