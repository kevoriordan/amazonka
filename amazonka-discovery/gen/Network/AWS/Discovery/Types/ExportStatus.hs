{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.Types.ExportStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Discovery.Types.ExportStatus (
  ExportStatus (
    ..
    , Failed
    , InProgress
    , Succeeded
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ExportStatus = ExportStatus' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern Failed :: ExportStatus
pattern Failed = ExportStatus' "FAILED"

pattern InProgress :: ExportStatus
pattern InProgress = ExportStatus' "IN_PROGRESS"

pattern Succeeded :: ExportStatus
pattern Succeeded = ExportStatus' "SUCCEEDED"

{-# COMPLETE
  Failed,
  InProgress,
  Succeeded,
  ExportStatus' #-}

instance FromText ExportStatus where
    parser = (ExportStatus' . mk) <$> takeText

instance ToText ExportStatus where
    toText (ExportStatus' ci) = original ci

-- | Represents an enum of /known/ $ExportStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ExportStatus where
    toEnum i = case i of
        0 -> Failed
        1 -> InProgress
        2 -> Succeeded
        _ -> (error . showText) $ "Unknown index for ExportStatus: " <> toText i
    fromEnum x = case x of
        Failed -> 0
        InProgress -> 1
        Succeeded -> 2
        ExportStatus' name -> (error . showText) $ "Unknown ExportStatus: " <> original name

-- | Represents the bounds of /known/ $ExportStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ExportStatus where
    minBound = Failed
    maxBound = Succeeded

instance Hashable     ExportStatus
instance NFData       ExportStatus
instance ToByteString ExportStatus
instance ToQuery      ExportStatus
instance ToHeader     ExportStatus

instance FromJSON ExportStatus where
    parseJSON = parseJSONText "ExportStatus"
