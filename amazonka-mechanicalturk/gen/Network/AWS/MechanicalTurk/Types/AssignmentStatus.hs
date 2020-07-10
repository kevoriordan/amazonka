{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.Types.AssignmentStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MechanicalTurk.Types.AssignmentStatus (
  AssignmentStatus (
    ..
    , Approved
    , Rejected
    , Submitted
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AssignmentStatus = AssignmentStatus' (CI Text)
                          deriving (Eq, Ord, Read, Show, Data, Typeable,
                                    Generic)

pattern Approved :: AssignmentStatus
pattern Approved = AssignmentStatus' "Approved"

pattern Rejected :: AssignmentStatus
pattern Rejected = AssignmentStatus' "Rejected"

pattern Submitted :: AssignmentStatus
pattern Submitted = AssignmentStatus' "Submitted"

{-# COMPLETE
  Approved,
  Rejected,
  Submitted,
  AssignmentStatus' #-}

instance FromText AssignmentStatus where
    parser = (AssignmentStatus' . mk) <$> takeText

instance ToText AssignmentStatus where
    toText (AssignmentStatus' ci) = original ci

-- | Represents an enum of /known/ $AssignmentStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AssignmentStatus where
    toEnum i = case i of
        0 -> Approved
        1 -> Rejected
        2 -> Submitted
        _ -> (error . showText) $ "Unknown index for AssignmentStatus: " <> toText i
    fromEnum x = case x of
        Approved -> 0
        Rejected -> 1
        Submitted -> 2
        AssignmentStatus' name -> (error . showText) $ "Unknown AssignmentStatus: " <> original name

-- | Represents the bounds of /known/ $AssignmentStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AssignmentStatus where
    minBound = Approved
    maxBound = Submitted

instance Hashable     AssignmentStatus
instance NFData       AssignmentStatus
instance ToByteString AssignmentStatus
instance ToQuery      AssignmentStatus
instance ToHeader     AssignmentStatus

instance ToJSON AssignmentStatus where
    toJSON = toJSONText

instance FromJSON AssignmentStatus where
    parseJSON = parseJSONText "AssignmentStatus"
