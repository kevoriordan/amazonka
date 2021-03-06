{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types.ScanBy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudWatch.Types.ScanBy (
  ScanBy (
    ..
    , TimestampAscending
    , TimestampDescending
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ScanBy = ScanBy' (CI Text)
                deriving (Eq, Ord, Read, Show, Data, Typeable,
                          Generic)

pattern TimestampAscending :: ScanBy
pattern TimestampAscending = ScanBy' "TimestampAscending"

pattern TimestampDescending :: ScanBy
pattern TimestampDescending = ScanBy' "TimestampDescending"

{-# COMPLETE
  TimestampAscending,
  TimestampDescending,
  ScanBy' #-}

instance FromText ScanBy where
    parser = (ScanBy' . mk) <$> takeText

instance ToText ScanBy where
    toText (ScanBy' ci) = original ci

-- | Represents an enum of /known/ $ScanBy.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ScanBy where
    toEnum i = case i of
        0 -> TimestampAscending
        1 -> TimestampDescending
        _ -> (error . showText) $ "Unknown index for ScanBy: " <> toText i
    fromEnum x = case x of
        TimestampAscending -> 0
        TimestampDescending -> 1
        ScanBy' name -> (error . showText) $ "Unknown ScanBy: " <> original name

-- | Represents the bounds of /known/ $ScanBy.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ScanBy where
    minBound = TimestampAscending
    maxBound = TimestampDescending

instance Hashable     ScanBy
instance NFData       ScanBy
instance ToByteString ScanBy
instance ToQuery      ScanBy
instance ToHeader     ScanBy
