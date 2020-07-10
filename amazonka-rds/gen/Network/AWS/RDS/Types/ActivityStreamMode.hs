{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.ActivityStreamMode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.RDS.Types.ActivityStreamMode (
  ActivityStreamMode (
    ..
    , Async
    , Sync
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ActivityStreamMode = ActivityStreamMode' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern Async :: ActivityStreamMode
pattern Async = ActivityStreamMode' "async"

pattern Sync :: ActivityStreamMode
pattern Sync = ActivityStreamMode' "sync"

{-# COMPLETE
  Async,
  Sync,
  ActivityStreamMode' #-}

instance FromText ActivityStreamMode where
    parser = (ActivityStreamMode' . mk) <$> takeText

instance ToText ActivityStreamMode where
    toText (ActivityStreamMode' ci) = original ci

-- | Represents an enum of /known/ $ActivityStreamMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ActivityStreamMode where
    toEnum i = case i of
        0 -> Async
        1 -> Sync
        _ -> (error . showText) $ "Unknown index for ActivityStreamMode: " <> toText i
    fromEnum x = case x of
        Async -> 0
        Sync -> 1
        ActivityStreamMode' name -> (error . showText) $ "Unknown ActivityStreamMode: " <> original name

-- | Represents the bounds of /known/ $ActivityStreamMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ActivityStreamMode where
    minBound = Async
    maxBound = Sync

instance Hashable     ActivityStreamMode
instance NFData       ActivityStreamMode
instance ToByteString ActivityStreamMode
instance ToQuery      ActivityStreamMode
instance ToHeader     ActivityStreamMode

instance FromXML ActivityStreamMode where
    parseXML = parseXMLText "ActivityStreamMode"
