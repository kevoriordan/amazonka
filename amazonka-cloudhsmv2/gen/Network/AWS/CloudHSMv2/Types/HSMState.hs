{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSMv2.Types.HSMState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudHSMv2.Types.HSMState (
  HSMState (
    ..
    , HSActive
    , HSCreateInProgress
    , HSDegraded
    , HSDeleteInProgress
    , HSDeleted
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data HSMState = HSMState' (CI Text)
                  deriving (Eq, Ord, Read, Show, Data, Typeable,
                            Generic)

pattern HSActive :: HSMState
pattern HSActive = HSMState' "ACTIVE"

pattern HSCreateInProgress :: HSMState
pattern HSCreateInProgress = HSMState' "CREATE_IN_PROGRESS"

pattern HSDegraded :: HSMState
pattern HSDegraded = HSMState' "DEGRADED"

pattern HSDeleteInProgress :: HSMState
pattern HSDeleteInProgress = HSMState' "DELETE_IN_PROGRESS"

pattern HSDeleted :: HSMState
pattern HSDeleted = HSMState' "DELETED"

{-# COMPLETE
  HSActive,
  HSCreateInProgress,
  HSDegraded,
  HSDeleteInProgress,
  HSDeleted,
  HSMState' #-}

instance FromText HSMState where
    parser = (HSMState' . mk) <$> takeText

instance ToText HSMState where
    toText (HSMState' ci) = original ci

-- | Represents an enum of /known/ $HSMState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum HSMState where
    toEnum i = case i of
        0 -> HSActive
        1 -> HSCreateInProgress
        2 -> HSDegraded
        3 -> HSDeleteInProgress
        4 -> HSDeleted
        _ -> (error . showText) $ "Unknown index for HSMState: " <> toText i
    fromEnum x = case x of
        HSActive -> 0
        HSCreateInProgress -> 1
        HSDegraded -> 2
        HSDeleteInProgress -> 3
        HSDeleted -> 4
        HSMState' name -> (error . showText) $ "Unknown HSMState: " <> original name

-- | Represents the bounds of /known/ $HSMState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded HSMState where
    minBound = HSActive
    maxBound = HSDeleted

instance Hashable     HSMState
instance NFData       HSMState
instance ToByteString HSMState
instance ToQuery      HSMState
instance ToHeader     HSMState

instance FromJSON HSMState where
    parseJSON = parseJSONText "HSMState"
