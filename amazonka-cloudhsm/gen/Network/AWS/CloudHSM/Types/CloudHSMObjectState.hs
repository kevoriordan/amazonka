{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSM.Types.CloudHSMObjectState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudHSM.Types.CloudHSMObjectState (
  CloudHSMObjectState (
    ..
    , Degraded
    , Ready
    , Updating
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data CloudHSMObjectState = CloudHSMObjectState' (CI
                                                   Text)
                             deriving (Eq, Ord, Read, Show, Data, Typeable,
                                       Generic)

pattern Degraded :: CloudHSMObjectState
pattern Degraded = CloudHSMObjectState' "DEGRADED"

pattern Ready :: CloudHSMObjectState
pattern Ready = CloudHSMObjectState' "READY"

pattern Updating :: CloudHSMObjectState
pattern Updating = CloudHSMObjectState' "UPDATING"

{-# COMPLETE
  Degraded,
  Ready,
  Updating,
  CloudHSMObjectState' #-}

instance FromText CloudHSMObjectState where
    parser = (CloudHSMObjectState' . mk) <$> takeText

instance ToText CloudHSMObjectState where
    toText (CloudHSMObjectState' ci) = original ci

-- | Represents an enum of /known/ $CloudHSMObjectState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum CloudHSMObjectState where
    toEnum i = case i of
        0 -> Degraded
        1 -> Ready
        2 -> Updating
        _ -> (error . showText) $ "Unknown index for CloudHSMObjectState: " <> toText i
    fromEnum x = case x of
        Degraded -> 0
        Ready -> 1
        Updating -> 2
        CloudHSMObjectState' name -> (error . showText) $ "Unknown CloudHSMObjectState: " <> original name

-- | Represents the bounds of /known/ $CloudHSMObjectState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded CloudHSMObjectState where
    minBound = Degraded
    maxBound = Updating

instance Hashable     CloudHSMObjectState
instance NFData       CloudHSMObjectState
instance ToByteString CloudHSMObjectState
instance ToQuery      CloudHSMObjectState
instance ToHeader     CloudHSMObjectState

instance FromJSON CloudHSMObjectState where
    parseJSON = parseJSONText "CloudHSMObjectState"
