{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.Types.OrderBy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudWatchLogs.Types.OrderBy (
  OrderBy (
    ..
    , LastEventTime
    , LogStreamName
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data OrderBy = OrderBy' (CI Text)
                 deriving (Eq, Ord, Read, Show, Data, Typeable,
                           Generic)

pattern LastEventTime :: OrderBy
pattern LastEventTime = OrderBy' "LastEventTime"

pattern LogStreamName :: OrderBy
pattern LogStreamName = OrderBy' "LogStreamName"

{-# COMPLETE
  LastEventTime,
  LogStreamName,
  OrderBy' #-}

instance FromText OrderBy where
    parser = (OrderBy' . mk) <$> takeText

instance ToText OrderBy where
    toText (OrderBy' ci) = original ci

-- | Represents an enum of /known/ $OrderBy.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum OrderBy where
    toEnum i = case i of
        0 -> LastEventTime
        1 -> LogStreamName
        _ -> (error . showText) $ "Unknown index for OrderBy: " <> toText i
    fromEnum x = case x of
        LastEventTime -> 0
        LogStreamName -> 1
        OrderBy' name -> (error . showText) $ "Unknown OrderBy: " <> original name

-- | Represents the bounds of /known/ $OrderBy.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded OrderBy where
    minBound = LastEventTime
    maxBound = LogStreamName

instance Hashable     OrderBy
instance NFData       OrderBy
instance ToByteString OrderBy
instance ToQuery      OrderBy
instance ToHeader     OrderBy

instance ToJSON OrderBy where
    toJSON = toJSONText
