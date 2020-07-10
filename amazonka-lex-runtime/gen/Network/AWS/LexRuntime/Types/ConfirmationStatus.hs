{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexRuntime.Types.ConfirmationStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.LexRuntime.Types.ConfirmationStatus (
  ConfirmationStatus (
    ..
    , Confirmed
    , Denied
    , None
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ConfirmationStatus = ConfirmationStatus' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern Confirmed :: ConfirmationStatus
pattern Confirmed = ConfirmationStatus' "Confirmed"

pattern Denied :: ConfirmationStatus
pattern Denied = ConfirmationStatus' "Denied"

pattern None :: ConfirmationStatus
pattern None = ConfirmationStatus' "None"

{-# COMPLETE
  Confirmed,
  Denied,
  None,
  ConfirmationStatus' #-}

instance FromText ConfirmationStatus where
    parser = (ConfirmationStatus' . mk) <$> takeText

instance ToText ConfirmationStatus where
    toText (ConfirmationStatus' ci) = original ci

-- | Represents an enum of /known/ $ConfirmationStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ConfirmationStatus where
    toEnum i = case i of
        0 -> Confirmed
        1 -> Denied
        2 -> None
        _ -> (error . showText) $ "Unknown index for ConfirmationStatus: " <> toText i
    fromEnum x = case x of
        Confirmed -> 0
        Denied -> 1
        None -> 2
        ConfirmationStatus' name -> (error . showText) $ "Unknown ConfirmationStatus: " <> original name

-- | Represents the bounds of /known/ $ConfirmationStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ConfirmationStatus where
    minBound = Confirmed
    maxBound = None

instance Hashable     ConfirmationStatus
instance NFData       ConfirmationStatus
instance ToByteString ConfirmationStatus
instance ToQuery      ConfirmationStatus
instance ToHeader     ConfirmationStatus

instance ToJSON ConfirmationStatus where
    toJSON = toJSONText

instance FromJSON ConfirmationStatus where
    parseJSON = parseJSONText "ConfirmationStatus"
