{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.PatchDeploymentStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.PatchDeploymentStatus (
  PatchDeploymentStatus (
    ..
    , Approved
    , ExplicitApproved
    , ExplicitRejected
    , PendingApproval
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data PatchDeploymentStatus = PatchDeploymentStatus' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern Approved :: PatchDeploymentStatus
pattern Approved = PatchDeploymentStatus' "APPROVED"

pattern ExplicitApproved :: PatchDeploymentStatus
pattern ExplicitApproved = PatchDeploymentStatus' "EXPLICIT_APPROVED"

pattern ExplicitRejected :: PatchDeploymentStatus
pattern ExplicitRejected = PatchDeploymentStatus' "EXPLICIT_REJECTED"

pattern PendingApproval :: PatchDeploymentStatus
pattern PendingApproval = PatchDeploymentStatus' "PENDING_APPROVAL"

{-# COMPLETE
  Approved,
  ExplicitApproved,
  ExplicitRejected,
  PendingApproval,
  PatchDeploymentStatus' #-}

instance FromText PatchDeploymentStatus where
    parser = (PatchDeploymentStatus' . mk) <$> takeText

instance ToText PatchDeploymentStatus where
    toText (PatchDeploymentStatus' ci) = original ci

-- | Represents an enum of /known/ $PatchDeploymentStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum PatchDeploymentStatus where
    toEnum i = case i of
        0 -> Approved
        1 -> ExplicitApproved
        2 -> ExplicitRejected
        3 -> PendingApproval
        _ -> (error . showText) $ "Unknown index for PatchDeploymentStatus: " <> toText i
    fromEnum x = case x of
        Approved -> 0
        ExplicitApproved -> 1
        ExplicitRejected -> 2
        PendingApproval -> 3
        PatchDeploymentStatus' name -> (error . showText) $ "Unknown PatchDeploymentStatus: " <> original name

-- | Represents the bounds of /known/ $PatchDeploymentStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded PatchDeploymentStatus where
    minBound = Approved
    maxBound = PendingApproval

instance Hashable     PatchDeploymentStatus
instance NFData       PatchDeploymentStatus
instance ToByteString PatchDeploymentStatus
instance ToQuery      PatchDeploymentStatus
instance ToHeader     PatchDeploymentStatus

instance FromJSON PatchDeploymentStatus where
    parseJSON = parseJSONText "PatchDeploymentStatus"
