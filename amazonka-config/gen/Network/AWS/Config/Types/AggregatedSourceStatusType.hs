{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.AggregatedSourceStatusType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Config.Types.AggregatedSourceStatusType (
  AggregatedSourceStatusType (
    ..
    , ASSTFailed
    , ASSTOutdated
    , ASSTSucceeded
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AggregatedSourceStatusType = AggregatedSourceStatusType' (CI
                                                                 Text)
                                    deriving (Eq, Ord, Read, Show, Data,
                                              Typeable, Generic)

pattern ASSTFailed :: AggregatedSourceStatusType
pattern ASSTFailed = AggregatedSourceStatusType' "FAILED"

pattern ASSTOutdated :: AggregatedSourceStatusType
pattern ASSTOutdated = AggregatedSourceStatusType' "OUTDATED"

pattern ASSTSucceeded :: AggregatedSourceStatusType
pattern ASSTSucceeded = AggregatedSourceStatusType' "SUCCEEDED"

{-# COMPLETE
  ASSTFailed,
  ASSTOutdated,
  ASSTSucceeded,
  AggregatedSourceStatusType' #-}

instance FromText AggregatedSourceStatusType where
    parser = (AggregatedSourceStatusType' . mk) <$> takeText

instance ToText AggregatedSourceStatusType where
    toText (AggregatedSourceStatusType' ci) = original ci

-- | Represents an enum of /known/ $AggregatedSourceStatusType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AggregatedSourceStatusType where
    toEnum i = case i of
        0 -> ASSTFailed
        1 -> ASSTOutdated
        2 -> ASSTSucceeded
        _ -> (error . showText) $ "Unknown index for AggregatedSourceStatusType: " <> toText i
    fromEnum x = case x of
        ASSTFailed -> 0
        ASSTOutdated -> 1
        ASSTSucceeded -> 2
        AggregatedSourceStatusType' name -> (error . showText) $ "Unknown AggregatedSourceStatusType: " <> original name

-- | Represents the bounds of /known/ $AggregatedSourceStatusType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AggregatedSourceStatusType where
    minBound = ASSTFailed
    maxBound = ASSTSucceeded

instance Hashable     AggregatedSourceStatusType
instance NFData       AggregatedSourceStatusType
instance ToByteString AggregatedSourceStatusType
instance ToQuery      AggregatedSourceStatusType
instance ToHeader     AggregatedSourceStatusType

instance ToJSON AggregatedSourceStatusType where
    toJSON = toJSONText

instance FromJSON AggregatedSourceStatusType where
    parseJSON = parseJSONText "AggregatedSourceStatusType"
