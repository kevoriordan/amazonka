{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.AssociationExecutionFilterKey
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.AssociationExecutionFilterKey (
  AssociationExecutionFilterKey (
    ..
    , AssocEFKCreatedTime
    , AssocEFKExecutionId
    , AssocEFKStatus
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AssociationExecutionFilterKey = AssociationExecutionFilterKey' (CI
                                                                       Text)
                                       deriving (Eq, Ord, Read, Show, Data,
                                                 Typeable, Generic)

pattern AssocEFKCreatedTime :: AssociationExecutionFilterKey
pattern AssocEFKCreatedTime = AssociationExecutionFilterKey' "CreatedTime"

pattern AssocEFKExecutionId :: AssociationExecutionFilterKey
pattern AssocEFKExecutionId = AssociationExecutionFilterKey' "ExecutionId"

pattern AssocEFKStatus :: AssociationExecutionFilterKey
pattern AssocEFKStatus = AssociationExecutionFilterKey' "Status"

{-# COMPLETE
  AssocEFKCreatedTime,
  AssocEFKExecutionId,
  AssocEFKStatus,
  AssociationExecutionFilterKey' #-}

instance FromText AssociationExecutionFilterKey where
    parser = (AssociationExecutionFilterKey' . mk) <$> takeText

instance ToText AssociationExecutionFilterKey where
    toText (AssociationExecutionFilterKey' ci) = original ci

-- | Represents an enum of /known/ $AssociationExecutionFilterKey.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AssociationExecutionFilterKey where
    toEnum i = case i of
        0 -> AssocEFKCreatedTime
        1 -> AssocEFKExecutionId
        2 -> AssocEFKStatus
        _ -> (error . showText) $ "Unknown index for AssociationExecutionFilterKey: " <> toText i
    fromEnum x = case x of
        AssocEFKCreatedTime -> 0
        AssocEFKExecutionId -> 1
        AssocEFKStatus -> 2
        AssociationExecutionFilterKey' name -> (error . showText) $ "Unknown AssociationExecutionFilterKey: " <> original name

-- | Represents the bounds of /known/ $AssociationExecutionFilterKey.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AssociationExecutionFilterKey where
    minBound = AssocEFKCreatedTime
    maxBound = AssocEFKStatus

instance Hashable     AssociationExecutionFilterKey
instance NFData       AssociationExecutionFilterKey
instance ToByteString AssociationExecutionFilterKey
instance ToQuery      AssociationExecutionFilterKey
instance ToHeader     AssociationExecutionFilterKey

instance ToJSON AssociationExecutionFilterKey where
    toJSON = toJSONText
