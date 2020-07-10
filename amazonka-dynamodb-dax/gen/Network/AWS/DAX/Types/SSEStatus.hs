{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.Types.SSEStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DAX.Types.SSEStatus (
  SSEStatus (
    ..
    , Disabled
    , Disabling
    , Enabled
    , Enabling
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data SSEStatus = SSEStatus' (CI Text)
                   deriving (Eq, Ord, Read, Show, Data, Typeable,
                             Generic)

pattern Disabled :: SSEStatus
pattern Disabled = SSEStatus' "DISABLED"

pattern Disabling :: SSEStatus
pattern Disabling = SSEStatus' "DISABLING"

pattern Enabled :: SSEStatus
pattern Enabled = SSEStatus' "ENABLED"

pattern Enabling :: SSEStatus
pattern Enabling = SSEStatus' "ENABLING"

{-# COMPLETE
  Disabled,
  Disabling,
  Enabled,
  Enabling,
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
        0 -> Disabled
        1 -> Disabling
        2 -> Enabled
        3 -> Enabling
        _ -> (error . showText) $ "Unknown index for SSEStatus: " <> toText i
    fromEnum x = case x of
        Disabled -> 0
        Disabling -> 1
        Enabled -> 2
        Enabling -> 3
        SSEStatus' name -> (error . showText) $ "Unknown SSEStatus: " <> original name

-- | Represents the bounds of /known/ $SSEStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded SSEStatus where
    minBound = Disabled
    maxBound = Enabling

instance Hashable     SSEStatus
instance NFData       SSEStatus
instance ToByteString SSEStatus
instance ToQuery      SSEStatus
instance ToHeader     SSEStatus

instance FromJSON SSEStatus where
    parseJSON = parseJSONText "SSEStatus"
