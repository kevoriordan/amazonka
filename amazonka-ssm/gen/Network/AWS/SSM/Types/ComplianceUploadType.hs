{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ComplianceUploadType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.ComplianceUploadType (
  ComplianceUploadType (
    ..
    , Complete
    , Partial
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ComplianceUploadType = ComplianceUploadType' (CI
                                                     Text)
                              deriving (Eq, Ord, Read, Show, Data, Typeable,
                                        Generic)

pattern Complete :: ComplianceUploadType
pattern Complete = ComplianceUploadType' "COMPLETE"

pattern Partial :: ComplianceUploadType
pattern Partial = ComplianceUploadType' "PARTIAL"

{-# COMPLETE
  Complete,
  Partial,
  ComplianceUploadType' #-}

instance FromText ComplianceUploadType where
    parser = (ComplianceUploadType' . mk) <$> takeText

instance ToText ComplianceUploadType where
    toText (ComplianceUploadType' ci) = original ci

-- | Represents an enum of /known/ $ComplianceUploadType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ComplianceUploadType where
    toEnum i = case i of
        0 -> Complete
        1 -> Partial
        _ -> (error . showText) $ "Unknown index for ComplianceUploadType: " <> toText i
    fromEnum x = case x of
        Complete -> 0
        Partial -> 1
        ComplianceUploadType' name -> (error . showText) $ "Unknown ComplianceUploadType: " <> original name

-- | Represents the bounds of /known/ $ComplianceUploadType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ComplianceUploadType where
    minBound = Complete
    maxBound = Partial

instance Hashable     ComplianceUploadType
instance NFData       ComplianceUploadType
instance ToByteString ComplianceUploadType
instance ToQuery      ComplianceUploadType
instance ToHeader     ComplianceUploadType

instance ToJSON ComplianceUploadType where
    toJSON = toJSONText
