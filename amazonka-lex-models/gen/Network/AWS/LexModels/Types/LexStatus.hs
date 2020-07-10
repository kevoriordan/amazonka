{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.LexStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.LexModels.Types.LexStatus (
  LexStatus (
    ..
    , LSBuilding
    , LSFailed
    , LSNotBuilt
    , LSReady
    , LSReadyBasicTesting
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data LexStatus = LexStatus' (CI Text)
                   deriving (Eq, Ord, Read, Show, Data, Typeable,
                             Generic)

pattern LSBuilding :: LexStatus
pattern LSBuilding = LexStatus' "BUILDING"

pattern LSFailed :: LexStatus
pattern LSFailed = LexStatus' "FAILED"

pattern LSNotBuilt :: LexStatus
pattern LSNotBuilt = LexStatus' "NOT_BUILT"

pattern LSReady :: LexStatus
pattern LSReady = LexStatus' "READY"

pattern LSReadyBasicTesting :: LexStatus
pattern LSReadyBasicTesting = LexStatus' "READY_BASIC_TESTING"

{-# COMPLETE
  LSBuilding,
  LSFailed,
  LSNotBuilt,
  LSReady,
  LSReadyBasicTesting,
  LexStatus' #-}

instance FromText LexStatus where
    parser = (LexStatus' . mk) <$> takeText

instance ToText LexStatus where
    toText (LexStatus' ci) = original ci

-- | Represents an enum of /known/ $LexStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum LexStatus where
    toEnum i = case i of
        0 -> LSBuilding
        1 -> LSFailed
        2 -> LSNotBuilt
        3 -> LSReady
        4 -> LSReadyBasicTesting
        _ -> (error . showText) $ "Unknown index for LexStatus: " <> toText i
    fromEnum x = case x of
        LSBuilding -> 0
        LSFailed -> 1
        LSNotBuilt -> 2
        LSReady -> 3
        LSReadyBasicTesting -> 4
        LexStatus' name -> (error . showText) $ "Unknown LexStatus: " <> original name

-- | Represents the bounds of /known/ $LexStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded LexStatus where
    minBound = LSBuilding
    maxBound = LSReadyBasicTesting

instance Hashable     LexStatus
instance NFData       LexStatus
instance ToByteString LexStatus
instance ToQuery      LexStatus
instance ToHeader     LexStatus

instance FromJSON LexStatus where
    parseJSON = parseJSONText "LexStatus"
