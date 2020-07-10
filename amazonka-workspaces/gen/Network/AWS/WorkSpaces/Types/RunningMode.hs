{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.RunningMode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkSpaces.Types.RunningMode (
  RunningMode (
    ..
    , AlwaysOn
    , AutoStop
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data RunningMode = RunningMode' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern AlwaysOn :: RunningMode
pattern AlwaysOn = RunningMode' "ALWAYS_ON"

pattern AutoStop :: RunningMode
pattern AutoStop = RunningMode' "AUTO_STOP"

{-# COMPLETE
  AlwaysOn,
  AutoStop,
  RunningMode' #-}

instance FromText RunningMode where
    parser = (RunningMode' . mk) <$> takeText

instance ToText RunningMode where
    toText (RunningMode' ci) = original ci

-- | Represents an enum of /known/ $RunningMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum RunningMode where
    toEnum i = case i of
        0 -> AlwaysOn
        1 -> AutoStop
        _ -> (error . showText) $ "Unknown index for RunningMode: " <> toText i
    fromEnum x = case x of
        AlwaysOn -> 0
        AutoStop -> 1
        RunningMode' name -> (error . showText) $ "Unknown RunningMode: " <> original name

-- | Represents the bounds of /known/ $RunningMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded RunningMode where
    minBound = AlwaysOn
    maxBound = AutoStop

instance Hashable     RunningMode
instance NFData       RunningMode
instance ToByteString RunningMode
instance ToQuery      RunningMode
instance ToHeader     RunningMode

instance ToJSON RunningMode where
    toJSON = toJSONText

instance FromJSON RunningMode where
    parseJSON = parseJSONText "RunningMode"
