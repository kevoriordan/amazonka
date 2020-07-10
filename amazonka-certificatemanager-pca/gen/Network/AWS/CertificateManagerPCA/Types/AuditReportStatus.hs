{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.Types.AuditReportStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CertificateManagerPCA.Types.AuditReportStatus (
  AuditReportStatus (
    ..
    , ARSCreating
    , ARSFailed
    , ARSSuccess
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AuditReportStatus = AuditReportStatus' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern ARSCreating :: AuditReportStatus
pattern ARSCreating = AuditReportStatus' "CREATING"

pattern ARSFailed :: AuditReportStatus
pattern ARSFailed = AuditReportStatus' "FAILED"

pattern ARSSuccess :: AuditReportStatus
pattern ARSSuccess = AuditReportStatus' "SUCCESS"

{-# COMPLETE
  ARSCreating,
  ARSFailed,
  ARSSuccess,
  AuditReportStatus' #-}

instance FromText AuditReportStatus where
    parser = (AuditReportStatus' . mk) <$> takeText

instance ToText AuditReportStatus where
    toText (AuditReportStatus' ci) = original ci

-- | Represents an enum of /known/ $AuditReportStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AuditReportStatus where
    toEnum i = case i of
        0 -> ARSCreating
        1 -> ARSFailed
        2 -> ARSSuccess
        _ -> (error . showText) $ "Unknown index for AuditReportStatus: " <> toText i
    fromEnum x = case x of
        ARSCreating -> 0
        ARSFailed -> 1
        ARSSuccess -> 2
        AuditReportStatus' name -> (error . showText) $ "Unknown AuditReportStatus: " <> original name

-- | Represents the bounds of /known/ $AuditReportStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AuditReportStatus where
    minBound = ARSCreating
    maxBound = ARSSuccess

instance Hashable     AuditReportStatus
instance NFData       AuditReportStatus
instance ToByteString AuditReportStatus
instance ToQuery      AuditReportStatus
instance ToHeader     AuditReportStatus

instance FromJSON AuditReportStatus where
    parseJSON = parseJSONText "AuditReportStatus"
