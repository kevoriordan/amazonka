{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.BuildStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GameLift.Types.BuildStatus (
  BuildStatus (
    ..
    , Failed
    , Initialized
    , Ready
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data BuildStatus = BuildStatus' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern Failed :: BuildStatus
pattern Failed = BuildStatus' "FAILED"

pattern Initialized :: BuildStatus
pattern Initialized = BuildStatus' "INITIALIZED"

pattern Ready :: BuildStatus
pattern Ready = BuildStatus' "READY"

{-# COMPLETE
  Failed,
  Initialized,
  Ready,
  BuildStatus' #-}

instance FromText BuildStatus where
    parser = (BuildStatus' . mk) <$> takeText

instance ToText BuildStatus where
    toText (BuildStatus' ci) = original ci

-- | Represents an enum of /known/ $BuildStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum BuildStatus where
    toEnum i = case i of
        0 -> Failed
        1 -> Initialized
        2 -> Ready
        _ -> (error . showText) $ "Unknown index for BuildStatus: " <> toText i
    fromEnum x = case x of
        Failed -> 0
        Initialized -> 1
        Ready -> 2
        BuildStatus' name -> (error . showText) $ "Unknown BuildStatus: " <> original name

-- | Represents the bounds of /known/ $BuildStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded BuildStatus where
    minBound = Failed
    maxBound = Ready

instance Hashable     BuildStatus
instance NFData       BuildStatus
instance ToByteString BuildStatus
instance ToQuery      BuildStatus
instance ToHeader     BuildStatus

instance ToJSON BuildStatus where
    toJSON = toJSONText

instance FromJSON BuildStatus where
    parseJSON = parseJSONText "BuildStatus"
