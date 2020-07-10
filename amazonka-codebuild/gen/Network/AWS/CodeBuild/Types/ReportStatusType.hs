{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.ReportStatusType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeBuild.Types.ReportStatusType (
  ReportStatusType (
    ..
    , Deleting
    , Failed
    , Generating
    , Incomplete
    , Succeeded
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ReportStatusType = ReportStatusType' (CI Text)
                          deriving (Eq, Ord, Read, Show, Data, Typeable,
                                    Generic)

pattern Deleting :: ReportStatusType
pattern Deleting = ReportStatusType' "DELETING"

pattern Failed :: ReportStatusType
pattern Failed = ReportStatusType' "FAILED"

pattern Generating :: ReportStatusType
pattern Generating = ReportStatusType' "GENERATING"

pattern Incomplete :: ReportStatusType
pattern Incomplete = ReportStatusType' "INCOMPLETE"

pattern Succeeded :: ReportStatusType
pattern Succeeded = ReportStatusType' "SUCCEEDED"

{-# COMPLETE
  Deleting,
  Failed,
  Generating,
  Incomplete,
  Succeeded,
  ReportStatusType' #-}

instance FromText ReportStatusType where
    parser = (ReportStatusType' . mk) <$> takeText

instance ToText ReportStatusType where
    toText (ReportStatusType' ci) = original ci

-- | Represents an enum of /known/ $ReportStatusType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ReportStatusType where
    toEnum i = case i of
        0 -> Deleting
        1 -> Failed
        2 -> Generating
        3 -> Incomplete
        4 -> Succeeded
        _ -> (error . showText) $ "Unknown index for ReportStatusType: " <> toText i
    fromEnum x = case x of
        Deleting -> 0
        Failed -> 1
        Generating -> 2
        Incomplete -> 3
        Succeeded -> 4
        ReportStatusType' name -> (error . showText) $ "Unknown ReportStatusType: " <> original name

-- | Represents the bounds of /known/ $ReportStatusType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ReportStatusType where
    minBound = Deleting
    maxBound = Succeeded

instance Hashable     ReportStatusType
instance NFData       ReportStatusType
instance ToByteString ReportStatusType
instance ToQuery      ReportStatusType
instance ToHeader     ReportStatusType

instance ToJSON ReportStatusType where
    toJSON = toJSONText

instance FromJSON ReportStatusType where
    parseJSON = parseJSONText "ReportStatusType"
