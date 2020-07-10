{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.CACertificateStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.CACertificateStatus (
  CACertificateStatus (
    ..
    , CACSActive
    , CACSInactive
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data CACertificateStatus = CACertificateStatus' (CI
                                                   Text)
                             deriving (Eq, Ord, Read, Show, Data, Typeable,
                                       Generic)

pattern CACSActive :: CACertificateStatus
pattern CACSActive = CACertificateStatus' "ACTIVE"

pattern CACSInactive :: CACertificateStatus
pattern CACSInactive = CACertificateStatus' "INACTIVE"

{-# COMPLETE
  CACSActive,
  CACSInactive,
  CACertificateStatus' #-}

instance FromText CACertificateStatus where
    parser = (CACertificateStatus' . mk) <$> takeText

instance ToText CACertificateStatus where
    toText (CACertificateStatus' ci) = original ci

-- | Represents an enum of /known/ $CACertificateStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum CACertificateStatus where
    toEnum i = case i of
        0 -> CACSActive
        1 -> CACSInactive
        _ -> (error . showText) $ "Unknown index for CACertificateStatus: " <> toText i
    fromEnum x = case x of
        CACSActive -> 0
        CACSInactive -> 1
        CACertificateStatus' name -> (error . showText) $ "Unknown CACertificateStatus: " <> original name

-- | Represents the bounds of /known/ $CACertificateStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded CACertificateStatus where
    minBound = CACSActive
    maxBound = CACSInactive

instance Hashable     CACertificateStatus
instance NFData       CACertificateStatus
instance ToByteString CACertificateStatus
instance ToQuery      CACertificateStatus
instance ToHeader     CACertificateStatus

instance ToJSON CACertificateStatus where
    toJSON = toJSONText

instance FromJSON CACertificateStatus where
    parseJSON = parseJSONText "CACertificateStatus"
