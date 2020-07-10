{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.SupportedSavingsPlansType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CostExplorer.Types.SupportedSavingsPlansType (
  SupportedSavingsPlansType (
    ..
    , ComputeSp
    , EC2InstanceSp
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data SupportedSavingsPlansType = SupportedSavingsPlansType' (CI
                                                               Text)
                                   deriving (Eq, Ord, Read, Show, Data,
                                             Typeable, Generic)

pattern ComputeSp :: SupportedSavingsPlansType
pattern ComputeSp = SupportedSavingsPlansType' "COMPUTE_SP"

pattern EC2InstanceSp :: SupportedSavingsPlansType
pattern EC2InstanceSp = SupportedSavingsPlansType' "EC2_INSTANCE_SP"

{-# COMPLETE
  ComputeSp,
  EC2InstanceSp,
  SupportedSavingsPlansType' #-}

instance FromText SupportedSavingsPlansType where
    parser = (SupportedSavingsPlansType' . mk) <$> takeText

instance ToText SupportedSavingsPlansType where
    toText (SupportedSavingsPlansType' ci) = original ci

-- | Represents an enum of /known/ $SupportedSavingsPlansType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum SupportedSavingsPlansType where
    toEnum i = case i of
        0 -> ComputeSp
        1 -> EC2InstanceSp
        _ -> (error . showText) $ "Unknown index for SupportedSavingsPlansType: " <> toText i
    fromEnum x = case x of
        ComputeSp -> 0
        EC2InstanceSp -> 1
        SupportedSavingsPlansType' name -> (error . showText) $ "Unknown SupportedSavingsPlansType: " <> original name

-- | Represents the bounds of /known/ $SupportedSavingsPlansType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded SupportedSavingsPlansType where
    minBound = ComputeSp
    maxBound = EC2InstanceSp

instance Hashable     SupportedSavingsPlansType
instance NFData       SupportedSavingsPlansType
instance ToByteString SupportedSavingsPlansType
instance ToQuery      SupportedSavingsPlansType
instance ToHeader     SupportedSavingsPlansType

instance ToJSON SupportedSavingsPlansType where
    toJSON = toJSONText

instance FromJSON SupportedSavingsPlansType where
    parseJSON = parseJSONText "SupportedSavingsPlansType"
