{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.CrawlState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.CrawlState (
  CrawlState (
    ..
    , CSCancelled
    , CSFailed
    , CSRunning
    , CSSucceeded
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data CrawlState = CrawlState' (CI Text)
                    deriving (Eq, Ord, Read, Show, Data, Typeable,
                              Generic)

pattern CSCancelled :: CrawlState
pattern CSCancelled = CrawlState' "CANCELLED"

pattern CSFailed :: CrawlState
pattern CSFailed = CrawlState' "FAILED"

pattern CSRunning :: CrawlState
pattern CSRunning = CrawlState' "RUNNING"

pattern CSSucceeded :: CrawlState
pattern CSSucceeded = CrawlState' "SUCCEEDED"

{-# COMPLETE
  CSCancelled,
  CSFailed,
  CSRunning,
  CSSucceeded,
  CrawlState' #-}

instance FromText CrawlState where
    parser = (CrawlState' . mk) <$> takeText

instance ToText CrawlState where
    toText (CrawlState' ci) = original ci

-- | Represents an enum of /known/ $CrawlState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum CrawlState where
    toEnum i = case i of
        0 -> CSCancelled
        1 -> CSFailed
        2 -> CSRunning
        3 -> CSSucceeded
        _ -> (error . showText) $ "Unknown index for CrawlState: " <> toText i
    fromEnum x = case x of
        CSCancelled -> 0
        CSFailed -> 1
        CSRunning -> 2
        CSSucceeded -> 3
        CrawlState' name -> (error . showText) $ "Unknown CrawlState: " <> original name

-- | Represents the bounds of /known/ $CrawlState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded CrawlState where
    minBound = CSCancelled
    maxBound = CSSucceeded

instance Hashable     CrawlState
instance NFData       CrawlState
instance ToByteString CrawlState
instance ToQuery      CrawlState
instance ToHeader     CrawlState

instance ToJSON CrawlState where
    toJSON = toJSONText

instance FromJSON CrawlState where
    parseJSON = parseJSONText "CrawlState"
