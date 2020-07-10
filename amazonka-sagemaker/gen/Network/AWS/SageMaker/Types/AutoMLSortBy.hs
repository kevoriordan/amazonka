{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AutoMLSortBy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.AutoMLSortBy (
  AutoMLSortBy (
    ..
    , AMLSBCreationTime
    , AMLSBName
    , AMLSBStatus
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AutoMLSortBy = AutoMLSortBy' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern AMLSBCreationTime :: AutoMLSortBy
pattern AMLSBCreationTime = AutoMLSortBy' "CreationTime"

pattern AMLSBName :: AutoMLSortBy
pattern AMLSBName = AutoMLSortBy' "Name"

pattern AMLSBStatus :: AutoMLSortBy
pattern AMLSBStatus = AutoMLSortBy' "Status"

{-# COMPLETE
  AMLSBCreationTime,
  AMLSBName,
  AMLSBStatus,
  AutoMLSortBy' #-}

instance FromText AutoMLSortBy where
    parser = (AutoMLSortBy' . mk) <$> takeText

instance ToText AutoMLSortBy where
    toText (AutoMLSortBy' ci) = original ci

-- | Represents an enum of /known/ $AutoMLSortBy.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AutoMLSortBy where
    toEnum i = case i of
        0 -> AMLSBCreationTime
        1 -> AMLSBName
        2 -> AMLSBStatus
        _ -> (error . showText) $ "Unknown index for AutoMLSortBy: " <> toText i
    fromEnum x = case x of
        AMLSBCreationTime -> 0
        AMLSBName -> 1
        AMLSBStatus -> 2
        AutoMLSortBy' name -> (error . showText) $ "Unknown AutoMLSortBy: " <> original name

-- | Represents the bounds of /known/ $AutoMLSortBy.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AutoMLSortBy where
    minBound = AMLSBCreationTime
    maxBound = AMLSBStatus

instance Hashable     AutoMLSortBy
instance NFData       AutoMLSortBy
instance ToByteString AutoMLSortBy
instance ToQuery      AutoMLSortBy
instance ToHeader     AutoMLSortBy

instance ToJSON AutoMLSortBy where
    toJSON = toJSONText
