{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.AssociationExecutionTargetsFilterKey
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.AssociationExecutionTargetsFilterKey (
  AssociationExecutionTargetsFilterKey (
    ..
    , AssocETFKResourceId
    , AssocETFKResourceType
    , AssocETFKStatus
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AssociationExecutionTargetsFilterKey = AssociationExecutionTargetsFilterKey' (CI
                                                                                     Text)
                                              deriving (Eq, Ord, Read, Show,
                                                        Data, Typeable, Generic)

pattern AssocETFKResourceId :: AssociationExecutionTargetsFilterKey
pattern AssocETFKResourceId = AssociationExecutionTargetsFilterKey' "ResourceId"

pattern AssocETFKResourceType :: AssociationExecutionTargetsFilterKey
pattern AssocETFKResourceType = AssociationExecutionTargetsFilterKey' "ResourceType"

pattern AssocETFKStatus :: AssociationExecutionTargetsFilterKey
pattern AssocETFKStatus = AssociationExecutionTargetsFilterKey' "Status"

{-# COMPLETE
  AssocETFKResourceId,
  AssocETFKResourceType,
  AssocETFKStatus,
  AssociationExecutionTargetsFilterKey' #-}

instance FromText AssociationExecutionTargetsFilterKey where
    parser = (AssociationExecutionTargetsFilterKey' . mk) <$> takeText

instance ToText AssociationExecutionTargetsFilterKey where
    toText (AssociationExecutionTargetsFilterKey' ci) = original ci

-- | Represents an enum of /known/ $AssociationExecutionTargetsFilterKey.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AssociationExecutionTargetsFilterKey where
    toEnum i = case i of
        0 -> AssocETFKResourceId
        1 -> AssocETFKResourceType
        2 -> AssocETFKStatus
        _ -> (error . showText) $ "Unknown index for AssociationExecutionTargetsFilterKey: " <> toText i
    fromEnum x = case x of
        AssocETFKResourceId -> 0
        AssocETFKResourceType -> 1
        AssocETFKStatus -> 2
        AssociationExecutionTargetsFilterKey' name -> (error . showText) $ "Unknown AssociationExecutionTargetsFilterKey: " <> original name

-- | Represents the bounds of /known/ $AssociationExecutionTargetsFilterKey.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AssociationExecutionTargetsFilterKey where
    minBound = AssocETFKResourceId
    maxBound = AssocETFKStatus

instance Hashable     AssociationExecutionTargetsFilterKey
instance NFData       AssociationExecutionTargetsFilterKey
instance ToByteString AssociationExecutionTargetsFilterKey
instance ToQuery      AssociationExecutionTargetsFilterKey
instance ToHeader     AssociationExecutionTargetsFilterKey

instance ToJSON AssociationExecutionTargetsFilterKey where
    toJSON = toJSONText
