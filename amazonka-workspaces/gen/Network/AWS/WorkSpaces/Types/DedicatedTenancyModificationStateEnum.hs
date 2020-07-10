{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.DedicatedTenancyModificationStateEnum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkSpaces.Types.DedicatedTenancyModificationStateEnum (
  DedicatedTenancyModificationStateEnum (
    ..
    , Completed
    , Failed
    , Pending
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data DedicatedTenancyModificationStateEnum = DedicatedTenancyModificationStateEnum' (CI
                                                                                       Text)
                                               deriving (Eq, Ord, Read, Show,
                                                         Data, Typeable,
                                                         Generic)

pattern Completed :: DedicatedTenancyModificationStateEnum
pattern Completed = DedicatedTenancyModificationStateEnum' "COMPLETED"

pattern Failed :: DedicatedTenancyModificationStateEnum
pattern Failed = DedicatedTenancyModificationStateEnum' "FAILED"

pattern Pending :: DedicatedTenancyModificationStateEnum
pattern Pending = DedicatedTenancyModificationStateEnum' "PENDING"

{-# COMPLETE
  Completed,
  Failed,
  Pending,
  DedicatedTenancyModificationStateEnum' #-}

instance FromText DedicatedTenancyModificationStateEnum where
    parser = (DedicatedTenancyModificationStateEnum' . mk) <$> takeText

instance ToText DedicatedTenancyModificationStateEnum where
    toText (DedicatedTenancyModificationStateEnum' ci) = original ci

-- | Represents an enum of /known/ $DedicatedTenancyModificationStateEnum.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DedicatedTenancyModificationStateEnum where
    toEnum i = case i of
        0 -> Completed
        1 -> Failed
        2 -> Pending
        _ -> (error . showText) $ "Unknown index for DedicatedTenancyModificationStateEnum: " <> toText i
    fromEnum x = case x of
        Completed -> 0
        Failed -> 1
        Pending -> 2
        DedicatedTenancyModificationStateEnum' name -> (error . showText) $ "Unknown DedicatedTenancyModificationStateEnum: " <> original name

-- | Represents the bounds of /known/ $DedicatedTenancyModificationStateEnum.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DedicatedTenancyModificationStateEnum where
    minBound = Completed
    maxBound = Pending

instance Hashable     DedicatedTenancyModificationStateEnum
instance NFData       DedicatedTenancyModificationStateEnum
instance ToByteString DedicatedTenancyModificationStateEnum
instance ToQuery      DedicatedTenancyModificationStateEnum
instance ToHeader     DedicatedTenancyModificationStateEnum

instance FromJSON DedicatedTenancyModificationStateEnum where
    parseJSON = parseJSONText "DedicatedTenancyModificationStateEnum"
