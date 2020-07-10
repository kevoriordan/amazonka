{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ElasticGpuState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ElasticGpuState (
  ElasticGpuState (
    ..
    , Attached
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data ElasticGpuState = ElasticGpuState' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern Attached :: ElasticGpuState
pattern Attached = ElasticGpuState' "ATTACHED"

{-# COMPLETE
  Attached,
  ElasticGpuState' #-}

instance FromText ElasticGpuState where
    parser = (ElasticGpuState' . mk) <$> takeText

instance ToText ElasticGpuState where
    toText (ElasticGpuState' ci) = original ci

-- | Represents an enum of /known/ $ElasticGpuState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ElasticGpuState where
    toEnum i = case i of
        0 -> Attached
        _ -> (error . showText) $ "Unknown index for ElasticGpuState: " <> toText i
    fromEnum x = case x of
        Attached -> 0
        ElasticGpuState' name -> (error . showText) $ "Unknown ElasticGpuState: " <> original name

-- | Represents the bounds of /known/ $ElasticGpuState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ElasticGpuState where
    minBound = Attached
    maxBound = Attached

instance Hashable     ElasticGpuState
instance NFData       ElasticGpuState
instance ToByteString ElasticGpuState
instance ToQuery      ElasticGpuState
instance ToHeader     ElasticGpuState

instance FromXML ElasticGpuState where
    parseXML = parseXMLText "ElasticGpuState"
