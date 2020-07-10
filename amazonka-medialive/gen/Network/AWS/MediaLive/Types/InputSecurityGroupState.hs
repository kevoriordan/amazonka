{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputSecurityGroupState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.InputSecurityGroupState (
  InputSecurityGroupState (
    ..
    , ISGSDeleted
    , ISGSIdle
    , ISGSInUse
    , ISGSUpdating
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Placeholder documentation for InputSecurityGroupState
data InputSecurityGroupState = InputSecurityGroupState' (CI
                                                           Text)
                                 deriving (Eq, Ord, Read, Show, Data, Typeable,
                                           Generic)

pattern ISGSDeleted :: InputSecurityGroupState
pattern ISGSDeleted = InputSecurityGroupState' "DELETED"

pattern ISGSIdle :: InputSecurityGroupState
pattern ISGSIdle = InputSecurityGroupState' "IDLE"

pattern ISGSInUse :: InputSecurityGroupState
pattern ISGSInUse = InputSecurityGroupState' "IN_USE"

pattern ISGSUpdating :: InputSecurityGroupState
pattern ISGSUpdating = InputSecurityGroupState' "UPDATING"

{-# COMPLETE
  ISGSDeleted,
  ISGSIdle,
  ISGSInUse,
  ISGSUpdating,
  InputSecurityGroupState' #-}

instance FromText InputSecurityGroupState where
    parser = (InputSecurityGroupState' . mk) <$> takeText

instance ToText InputSecurityGroupState where
    toText (InputSecurityGroupState' ci) = original ci

-- | Represents an enum of /known/ $InputSecurityGroupState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum InputSecurityGroupState where
    toEnum i = case i of
        0 -> ISGSDeleted
        1 -> ISGSIdle
        2 -> ISGSInUse
        3 -> ISGSUpdating
        _ -> (error . showText) $ "Unknown index for InputSecurityGroupState: " <> toText i
    fromEnum x = case x of
        ISGSDeleted -> 0
        ISGSIdle -> 1
        ISGSInUse -> 2
        ISGSUpdating -> 3
        InputSecurityGroupState' name -> (error . showText) $ "Unknown InputSecurityGroupState: " <> original name

-- | Represents the bounds of /known/ $InputSecurityGroupState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded InputSecurityGroupState where
    minBound = ISGSDeleted
    maxBound = ISGSUpdating

instance Hashable     InputSecurityGroupState
instance NFData       InputSecurityGroupState
instance ToByteString InputSecurityGroupState
instance ToQuery      InputSecurityGroupState
instance ToHeader     InputSecurityGroupState

instance FromJSON InputSecurityGroupState where
    parseJSON = parseJSONText "InputSecurityGroupState"
