{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.CancelStepsRequestStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.CancelStepsRequestStatus (
  CancelStepsRequestStatus (
    ..
    , CSRSFailed
    , CSRSSubmitted
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data CancelStepsRequestStatus = CancelStepsRequestStatus' (CI
                                                             Text)
                                  deriving (Eq, Ord, Read, Show, Data, Typeable,
                                            Generic)

pattern CSRSFailed :: CancelStepsRequestStatus
pattern CSRSFailed = CancelStepsRequestStatus' "FAILED"

pattern CSRSSubmitted :: CancelStepsRequestStatus
pattern CSRSSubmitted = CancelStepsRequestStatus' "SUBMITTED"

{-# COMPLETE
  CSRSFailed,
  CSRSSubmitted,
  CancelStepsRequestStatus' #-}

instance FromText CancelStepsRequestStatus where
    parser = (CancelStepsRequestStatus' . mk) <$> takeText

instance ToText CancelStepsRequestStatus where
    toText (CancelStepsRequestStatus' ci) = original ci

-- | Represents an enum of /known/ $CancelStepsRequestStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum CancelStepsRequestStatus where
    toEnum i = case i of
        0 -> CSRSFailed
        1 -> CSRSSubmitted
        _ -> (error . showText) $ "Unknown index for CancelStepsRequestStatus: " <> toText i
    fromEnum x = case x of
        CSRSFailed -> 0
        CSRSSubmitted -> 1
        CancelStepsRequestStatus' name -> (error . showText) $ "Unknown CancelStepsRequestStatus: " <> original name

-- | Represents the bounds of /known/ $CancelStepsRequestStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded CancelStepsRequestStatus where
    minBound = CSRSFailed
    maxBound = CSRSSubmitted

instance Hashable     CancelStepsRequestStatus
instance NFData       CancelStepsRequestStatus
instance ToByteString CancelStepsRequestStatus
instance ToQuery      CancelStepsRequestStatus
instance ToHeader     CancelStepsRequestStatus

instance FromJSON CancelStepsRequestStatus where
    parseJSON = parseJSONText "CancelStepsRequestStatus"
