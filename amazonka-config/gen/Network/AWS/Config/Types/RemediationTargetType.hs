{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.RemediationTargetType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Config.Types.RemediationTargetType (
  RemediationTargetType (
    ..
    , SsmDocument
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data RemediationTargetType = RemediationTargetType' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern SsmDocument :: RemediationTargetType
pattern SsmDocument = RemediationTargetType' "SSM_DOCUMENT"

{-# COMPLETE
  SsmDocument,
  RemediationTargetType' #-}

instance FromText RemediationTargetType where
    parser = (RemediationTargetType' . mk) <$> takeText

instance ToText RemediationTargetType where
    toText (RemediationTargetType' ci) = original ci

-- | Represents an enum of /known/ $RemediationTargetType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum RemediationTargetType where
    toEnum i = case i of
        0 -> SsmDocument
        _ -> (error . showText) $ "Unknown index for RemediationTargetType: " <> toText i
    fromEnum x = case x of
        SsmDocument -> 0
        RemediationTargetType' name -> (error . showText) $ "Unknown RemediationTargetType: " <> original name

-- | Represents the bounds of /known/ $RemediationTargetType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded RemediationTargetType where
    minBound = SsmDocument
    maxBound = SsmDocument

instance Hashable     RemediationTargetType
instance NFData       RemediationTargetType
instance ToByteString RemediationTargetType
instance ToQuery      RemediationTargetType
instance ToHeader     RemediationTargetType

instance ToJSON RemediationTargetType where
    toJSON = toJSONText

instance FromJSON RemediationTargetType where
    parseJSON = parseJSONText "RemediationTargetType"
