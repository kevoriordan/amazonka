{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.TestGridSessionStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DeviceFarm.Types.TestGridSessionStatus (
  TestGridSessionStatus (
    ..
    , Active
    , Closed
    , Errored
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data TestGridSessionStatus = TestGridSessionStatus' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern Active :: TestGridSessionStatus
pattern Active = TestGridSessionStatus' "ACTIVE"

pattern Closed :: TestGridSessionStatus
pattern Closed = TestGridSessionStatus' "CLOSED"

pattern Errored :: TestGridSessionStatus
pattern Errored = TestGridSessionStatus' "ERRORED"

{-# COMPLETE
  Active,
  Closed,
  Errored,
  TestGridSessionStatus' #-}

instance FromText TestGridSessionStatus where
    parser = (TestGridSessionStatus' . mk) <$> takeText

instance ToText TestGridSessionStatus where
    toText (TestGridSessionStatus' ci) = original ci

-- | Represents an enum of /known/ $TestGridSessionStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum TestGridSessionStatus where
    toEnum i = case i of
        0 -> Active
        1 -> Closed
        2 -> Errored
        _ -> (error . showText) $ "Unknown index for TestGridSessionStatus: " <> toText i
    fromEnum x = case x of
        Active -> 0
        Closed -> 1
        Errored -> 2
        TestGridSessionStatus' name -> (error . showText) $ "Unknown TestGridSessionStatus: " <> original name

-- | Represents the bounds of /known/ $TestGridSessionStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded TestGridSessionStatus where
    minBound = Active
    maxBound = Errored

instance Hashable     TestGridSessionStatus
instance NFData       TestGridSessionStatus
instance ToByteString TestGridSessionStatus
instance ToQuery      TestGridSessionStatus
instance ToHeader     TestGridSessionStatus

instance ToJSON TestGridSessionStatus where
    toJSON = toJSONText

instance FromJSON TestGridSessionStatus where
    parseJSON = parseJSONText "TestGridSessionStatus"
