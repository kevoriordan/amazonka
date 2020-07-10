{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.ProvisionedConcurrencyStatusEnum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lambda.Types.ProvisionedConcurrencyStatusEnum (
  ProvisionedConcurrencyStatusEnum (
    ..
    , PCSEFailed
    , PCSEInProgress
    , PCSEReady
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ProvisionedConcurrencyStatusEnum = ProvisionedConcurrencyStatusEnum' (CI
                                                                             Text)
                                          deriving (Eq, Ord, Read, Show, Data,
                                                    Typeable, Generic)

pattern PCSEFailed :: ProvisionedConcurrencyStatusEnum
pattern PCSEFailed = ProvisionedConcurrencyStatusEnum' "FAILED"

pattern PCSEInProgress :: ProvisionedConcurrencyStatusEnum
pattern PCSEInProgress = ProvisionedConcurrencyStatusEnum' "IN_PROGRESS"

pattern PCSEReady :: ProvisionedConcurrencyStatusEnum
pattern PCSEReady = ProvisionedConcurrencyStatusEnum' "READY"

{-# COMPLETE
  PCSEFailed,
  PCSEInProgress,
  PCSEReady,
  ProvisionedConcurrencyStatusEnum' #-}

instance FromText ProvisionedConcurrencyStatusEnum where
    parser = (ProvisionedConcurrencyStatusEnum' . mk) <$> takeText

instance ToText ProvisionedConcurrencyStatusEnum where
    toText (ProvisionedConcurrencyStatusEnum' ci) = original ci

-- | Represents an enum of /known/ $ProvisionedConcurrencyStatusEnum.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ProvisionedConcurrencyStatusEnum where
    toEnum i = case i of
        0 -> PCSEFailed
        1 -> PCSEInProgress
        2 -> PCSEReady
        _ -> (error . showText) $ "Unknown index for ProvisionedConcurrencyStatusEnum: " <> toText i
    fromEnum x = case x of
        PCSEFailed -> 0
        PCSEInProgress -> 1
        PCSEReady -> 2
        ProvisionedConcurrencyStatusEnum' name -> (error . showText) $ "Unknown ProvisionedConcurrencyStatusEnum: " <> original name

-- | Represents the bounds of /known/ $ProvisionedConcurrencyStatusEnum.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ProvisionedConcurrencyStatusEnum where
    minBound = PCSEFailed
    maxBound = PCSEReady

instance Hashable     ProvisionedConcurrencyStatusEnum
instance NFData       ProvisionedConcurrencyStatusEnum
instance ToByteString ProvisionedConcurrencyStatusEnum
instance ToQuery      ProvisionedConcurrencyStatusEnum
instance ToHeader     ProvisionedConcurrencyStatusEnum

instance FromJSON ProvisionedConcurrencyStatusEnum where
    parseJSON = parseJSONText "ProvisionedConcurrencyStatusEnum"
