{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.DescribeActivationsFilterKeys
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.DescribeActivationsFilterKeys (
  DescribeActivationsFilterKeys (
    ..
    , ActivationIds
    , DefaultInstanceName
    , IAMRole
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data DescribeActivationsFilterKeys = DescribeActivationsFilterKeys' (CI
                                                                       Text)
                                       deriving (Eq, Ord, Read, Show, Data,
                                                 Typeable, Generic)

pattern ActivationIds :: DescribeActivationsFilterKeys
pattern ActivationIds = DescribeActivationsFilterKeys' "ActivationIds"

pattern DefaultInstanceName :: DescribeActivationsFilterKeys
pattern DefaultInstanceName = DescribeActivationsFilterKeys' "DefaultInstanceName"

pattern IAMRole :: DescribeActivationsFilterKeys
pattern IAMRole = DescribeActivationsFilterKeys' "IamRole"

{-# COMPLETE
  ActivationIds,
  DefaultInstanceName,
  IAMRole,
  DescribeActivationsFilterKeys' #-}

instance FromText DescribeActivationsFilterKeys where
    parser = (DescribeActivationsFilterKeys' . mk) <$> takeText

instance ToText DescribeActivationsFilterKeys where
    toText (DescribeActivationsFilterKeys' ci) = original ci

-- | Represents an enum of /known/ $DescribeActivationsFilterKeys.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DescribeActivationsFilterKeys where
    toEnum i = case i of
        0 -> ActivationIds
        1 -> DefaultInstanceName
        2 -> IAMRole
        _ -> (error . showText) $ "Unknown index for DescribeActivationsFilterKeys: " <> toText i
    fromEnum x = case x of
        ActivationIds -> 0
        DefaultInstanceName -> 1
        IAMRole -> 2
        DescribeActivationsFilterKeys' name -> (error . showText) $ "Unknown DescribeActivationsFilterKeys: " <> original name

-- | Represents the bounds of /known/ $DescribeActivationsFilterKeys.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DescribeActivationsFilterKeys where
    minBound = ActivationIds
    maxBound = IAMRole

instance Hashable     DescribeActivationsFilterKeys
instance NFData       DescribeActivationsFilterKeys
instance ToByteString DescribeActivationsFilterKeys
instance ToQuery      DescribeActivationsFilterKeys
instance ToHeader     DescribeActivationsFilterKeys

instance ToJSON DescribeActivationsFilterKeys where
    toJSON = toJSONText
