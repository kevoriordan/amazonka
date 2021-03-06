{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.ConflictDetectionType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AppSync.Types.ConflictDetectionType (
  ConflictDetectionType (
    ..
    , CDTNone
    , CDTVersion
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ConflictDetectionType = ConflictDetectionType' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern CDTNone :: ConflictDetectionType
pattern CDTNone = ConflictDetectionType' "NONE"

pattern CDTVersion :: ConflictDetectionType
pattern CDTVersion = ConflictDetectionType' "VERSION"

{-# COMPLETE
  CDTNone,
  CDTVersion,
  ConflictDetectionType' #-}

instance FromText ConflictDetectionType where
    parser = (ConflictDetectionType' . mk) <$> takeText

instance ToText ConflictDetectionType where
    toText (ConflictDetectionType' ci) = original ci

-- | Represents an enum of /known/ $ConflictDetectionType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ConflictDetectionType where
    toEnum i = case i of
        0 -> CDTNone
        1 -> CDTVersion
        _ -> (error . showText) $ "Unknown index for ConflictDetectionType: " <> toText i
    fromEnum x = case x of
        CDTNone -> 0
        CDTVersion -> 1
        ConflictDetectionType' name -> (error . showText) $ "Unknown ConflictDetectionType: " <> original name

-- | Represents the bounds of /known/ $ConflictDetectionType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ConflictDetectionType where
    minBound = CDTNone
    maxBound = CDTVersion

instance Hashable     ConflictDetectionType
instance NFData       ConflictDetectionType
instance ToByteString ConflictDetectionType
instance ToQuery      ConflictDetectionType
instance ToHeader     ConflictDetectionType

instance ToJSON ConflictDetectionType where
    toJSON = toJSONText

instance FromJSON ConflictDetectionType where
    parseJSON = parseJSONText "ConflictDetectionType"
