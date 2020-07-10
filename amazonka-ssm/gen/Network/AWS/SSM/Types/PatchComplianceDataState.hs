{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.PatchComplianceDataState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.PatchComplianceDataState (
  PatchComplianceDataState (
    ..
    , Failed
    , Installed
    , InstalledOther
    , InstalledPendingReboot
    , InstalledRejected
    , Missing
    , NotApplicable
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data PatchComplianceDataState = PatchComplianceDataState' (CI
                                                             Text)
                                  deriving (Eq, Ord, Read, Show, Data, Typeable,
                                            Generic)

pattern Failed :: PatchComplianceDataState
pattern Failed = PatchComplianceDataState' "FAILED"

pattern Installed :: PatchComplianceDataState
pattern Installed = PatchComplianceDataState' "INSTALLED"

pattern InstalledOther :: PatchComplianceDataState
pattern InstalledOther = PatchComplianceDataState' "INSTALLED_OTHER"

pattern InstalledPendingReboot :: PatchComplianceDataState
pattern InstalledPendingReboot = PatchComplianceDataState' "INSTALLED_PENDING_REBOOT"

pattern InstalledRejected :: PatchComplianceDataState
pattern InstalledRejected = PatchComplianceDataState' "INSTALLED_REJECTED"

pattern Missing :: PatchComplianceDataState
pattern Missing = PatchComplianceDataState' "MISSING"

pattern NotApplicable :: PatchComplianceDataState
pattern NotApplicable = PatchComplianceDataState' "NOT_APPLICABLE"

{-# COMPLETE
  Failed,
  Installed,
  InstalledOther,
  InstalledPendingReboot,
  InstalledRejected,
  Missing,
  NotApplicable,
  PatchComplianceDataState' #-}

instance FromText PatchComplianceDataState where
    parser = (PatchComplianceDataState' . mk) <$> takeText

instance ToText PatchComplianceDataState where
    toText (PatchComplianceDataState' ci) = original ci

-- | Represents an enum of /known/ $PatchComplianceDataState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum PatchComplianceDataState where
    toEnum i = case i of
        0 -> Failed
        1 -> Installed
        2 -> InstalledOther
        3 -> InstalledPendingReboot
        4 -> InstalledRejected
        5 -> Missing
        6 -> NotApplicable
        _ -> (error . showText) $ "Unknown index for PatchComplianceDataState: " <> toText i
    fromEnum x = case x of
        Failed -> 0
        Installed -> 1
        InstalledOther -> 2
        InstalledPendingReboot -> 3
        InstalledRejected -> 4
        Missing -> 5
        NotApplicable -> 6
        PatchComplianceDataState' name -> (error . showText) $ "Unknown PatchComplianceDataState: " <> original name

-- | Represents the bounds of /known/ $PatchComplianceDataState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded PatchComplianceDataState where
    minBound = Failed
    maxBound = NotApplicable

instance Hashable     PatchComplianceDataState
instance NFData       PatchComplianceDataState
instance ToByteString PatchComplianceDataState
instance ToQuery      PatchComplianceDataState
instance ToHeader     PatchComplianceDataState

instance FromJSON PatchComplianceDataState where
    parseJSON = parseJSONText "PatchComplianceDataState"
