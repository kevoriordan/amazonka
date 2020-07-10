{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroupsTagging.Types.ResourceErrorCode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ResourceGroupsTagging.Types.ResourceErrorCode (
  ResourceErrorCode (
    ..
    , InternalServiceException
    , InvalidParameterException
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ResourceErrorCode = ResourceErrorCode' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern InternalServiceException :: ResourceErrorCode
pattern InternalServiceException = ResourceErrorCode' "InternalServiceException"

pattern InvalidParameterException :: ResourceErrorCode
pattern InvalidParameterException = ResourceErrorCode' "InvalidParameterException"

{-# COMPLETE
  InternalServiceException,
  InvalidParameterException,
  ResourceErrorCode' #-}

instance FromText ResourceErrorCode where
    parser = (ResourceErrorCode' . mk) <$> takeText

instance ToText ResourceErrorCode where
    toText (ResourceErrorCode' ci) = original ci

-- | Represents an enum of /known/ $ResourceErrorCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ResourceErrorCode where
    toEnum i = case i of
        0 -> InternalServiceException
        1 -> InvalidParameterException
        _ -> (error . showText) $ "Unknown index for ResourceErrorCode: " <> toText i
    fromEnum x = case x of
        InternalServiceException -> 0
        InvalidParameterException -> 1
        ResourceErrorCode' name -> (error . showText) $ "Unknown ResourceErrorCode: " <> original name

-- | Represents the bounds of /known/ $ResourceErrorCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ResourceErrorCode where
    minBound = InternalServiceException
    maxBound = InvalidParameterException

instance Hashable     ResourceErrorCode
instance NFData       ResourceErrorCode
instance ToByteString ResourceErrorCode
instance ToQuery      ResourceErrorCode
instance ToHeader     ResourceErrorCode

instance FromJSON ResourceErrorCode where
    parseJSON = parseJSONText "ResourceErrorCode"
