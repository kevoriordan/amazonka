{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.Destination
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.LexModels.Types.Destination (
  Destination (
    ..
    , CloudwatchLogs
    , S3
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data Destination = Destination' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern CloudwatchLogs :: Destination
pattern CloudwatchLogs = Destination' "CLOUDWATCH_LOGS"

pattern S3 :: Destination
pattern S3 = Destination' "S3"

{-# COMPLETE
  CloudwatchLogs,
  S3,
  Destination' #-}

instance FromText Destination where
    parser = (Destination' . mk) <$> takeText

instance ToText Destination where
    toText (Destination' ci) = original ci

-- | Represents an enum of /known/ $Destination.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Destination where
    toEnum i = case i of
        0 -> CloudwatchLogs
        1 -> S3
        _ -> (error . showText) $ "Unknown index for Destination: " <> toText i
    fromEnum x = case x of
        CloudwatchLogs -> 0
        S3 -> 1
        Destination' name -> (error . showText) $ "Unknown Destination: " <> original name

-- | Represents the bounds of /known/ $Destination.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Destination where
    minBound = CloudwatchLogs
    maxBound = S3

instance Hashable     Destination
instance NFData       Destination
instance ToByteString Destination
instance ToQuery      Destination
instance ToHeader     Destination

instance ToJSON Destination where
    toJSON = toJSONText

instance FromJSON Destination where
    parseJSON = parseJSONText "Destination"
