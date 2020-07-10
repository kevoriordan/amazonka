{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ElasticGpuStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ElasticGpuStatus (
  ElasticGpuStatus (
    ..
    , EGSImpaired
    , EGSOK
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data ElasticGpuStatus = ElasticGpuStatus' (CI Text)
                          deriving (Eq, Ord, Read, Show, Data, Typeable,
                                    Generic)

pattern EGSImpaired :: ElasticGpuStatus
pattern EGSImpaired = ElasticGpuStatus' "IMPAIRED"

pattern EGSOK :: ElasticGpuStatus
pattern EGSOK = ElasticGpuStatus' "OK"

{-# COMPLETE
  EGSImpaired,
  EGSOK,
  ElasticGpuStatus' #-}

instance FromText ElasticGpuStatus where
    parser = (ElasticGpuStatus' . mk) <$> takeText

instance ToText ElasticGpuStatus where
    toText (ElasticGpuStatus' ci) = original ci

-- | Represents an enum of /known/ $ElasticGpuStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ElasticGpuStatus where
    toEnum i = case i of
        0 -> EGSImpaired
        1 -> EGSOK
        _ -> (error . showText) $ "Unknown index for ElasticGpuStatus: " <> toText i
    fromEnum x = case x of
        EGSImpaired -> 0
        EGSOK -> 1
        ElasticGpuStatus' name -> (error . showText) $ "Unknown ElasticGpuStatus: " <> original name

-- | Represents the bounds of /known/ $ElasticGpuStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ElasticGpuStatus where
    minBound = EGSImpaired
    maxBound = EGSOK

instance Hashable     ElasticGpuStatus
instance NFData       ElasticGpuStatus
instance ToByteString ElasticGpuStatus
instance ToQuery      ElasticGpuStatus
instance ToHeader     ElasticGpuStatus

instance FromXML ElasticGpuStatus where
    parseXML = parseXMLText "ElasticGpuStatus"
