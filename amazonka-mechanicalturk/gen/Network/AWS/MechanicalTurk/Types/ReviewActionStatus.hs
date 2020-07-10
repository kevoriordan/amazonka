{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.Types.ReviewActionStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MechanicalTurk.Types.ReviewActionStatus (
  ReviewActionStatus (
    ..
    , Cancelled
    , Failed
    , Intended
    , Succeeded
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ReviewActionStatus = ReviewActionStatus' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern Cancelled :: ReviewActionStatus
pattern Cancelled = ReviewActionStatus' "Cancelled"

pattern Failed :: ReviewActionStatus
pattern Failed = ReviewActionStatus' "Failed"

pattern Intended :: ReviewActionStatus
pattern Intended = ReviewActionStatus' "Intended"

pattern Succeeded :: ReviewActionStatus
pattern Succeeded = ReviewActionStatus' "Succeeded"

{-# COMPLETE
  Cancelled,
  Failed,
  Intended,
  Succeeded,
  ReviewActionStatus' #-}

instance FromText ReviewActionStatus where
    parser = (ReviewActionStatus' . mk) <$> takeText

instance ToText ReviewActionStatus where
    toText (ReviewActionStatus' ci) = original ci

-- | Represents an enum of /known/ $ReviewActionStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ReviewActionStatus where
    toEnum i = case i of
        0 -> Cancelled
        1 -> Failed
        2 -> Intended
        3 -> Succeeded
        _ -> (error . showText) $ "Unknown index for ReviewActionStatus: " <> toText i
    fromEnum x = case x of
        Cancelled -> 0
        Failed -> 1
        Intended -> 2
        Succeeded -> 3
        ReviewActionStatus' name -> (error . showText) $ "Unknown ReviewActionStatus: " <> original name

-- | Represents the bounds of /known/ $ReviewActionStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ReviewActionStatus where
    minBound = Cancelled
    maxBound = Succeeded

instance Hashable     ReviewActionStatus
instance NFData       ReviewActionStatus
instance ToByteString ReviewActionStatus
instance ToQuery      ReviewActionStatus
instance ToHeader     ReviewActionStatus

instance FromJSON ReviewActionStatus where
    parseJSON = parseJSONText "ReviewActionStatus"
