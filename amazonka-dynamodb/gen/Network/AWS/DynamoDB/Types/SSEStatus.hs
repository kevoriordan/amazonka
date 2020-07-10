{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.SSEStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Types.SSEStatus (
  SSEStatus (
    ..
    , SSESDisabled
    , SSESDisabling
    , SSESEnabled
    , SSESEnabling
    , SSESUpdating
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data SSEStatus = SSEStatus' (CI Text)
                   deriving (Eq, Ord, Read, Show, Data, Typeable,
                             Generic)

pattern SSESDisabled :: SSEStatus
pattern SSESDisabled = SSEStatus' "DISABLED"

pattern SSESDisabling :: SSEStatus
pattern SSESDisabling = SSEStatus' "DISABLING"

pattern SSESEnabled :: SSEStatus
pattern SSESEnabled = SSEStatus' "ENABLED"

pattern SSESEnabling :: SSEStatus
pattern SSESEnabling = SSEStatus' "ENABLING"

pattern SSESUpdating :: SSEStatus
pattern SSESUpdating = SSEStatus' "UPDATING"

{-# COMPLETE
  SSESDisabled,
  SSESDisabling,
  SSESEnabled,
  SSESEnabling,
  SSESUpdating,
  SSEStatus' #-}

instance FromText SSEStatus where
    parser = (SSEStatus' . mk) <$> takeText

instance ToText SSEStatus where
    toText (SSEStatus' ci) = original ci

-- | Represents an enum of /known/ $SSEStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum SSEStatus where
    toEnum i = case i of
        0 -> SSESDisabled
        1 -> SSESDisabling
        2 -> SSESEnabled
        3 -> SSESEnabling
        4 -> SSESUpdating
        _ -> (error . showText) $ "Unknown index for SSEStatus: " <> toText i
    fromEnum x = case x of
        SSESDisabled -> 0
        SSESDisabling -> 1
        SSESEnabled -> 2
        SSESEnabling -> 3
        SSESUpdating -> 4
        SSEStatus' name -> (error . showText) $ "Unknown SSEStatus: " <> original name

-- | Represents the bounds of /known/ $SSEStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded SSEStatus where
    minBound = SSESDisabled
    maxBound = SSESUpdating

instance Hashable     SSEStatus
instance NFData       SSEStatus
instance ToByteString SSEStatus
instance ToQuery      SSEStatus
instance ToHeader     SSEStatus

instance FromJSON SSEStatus where
    parseJSON = parseJSONText "SSEStatus"
