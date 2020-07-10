{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.Types.ImportTaskFilterName
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Discovery.Types.ImportTaskFilterName (
  ImportTaskFilterName (
    ..
    , ITFNImportTaskId
    , ITFNName
    , ITFNStatus
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ImportTaskFilterName = ImportTaskFilterName' (CI
                                                     Text)
                              deriving (Eq, Ord, Read, Show, Data, Typeable,
                                        Generic)

pattern ITFNImportTaskId :: ImportTaskFilterName
pattern ITFNImportTaskId = ImportTaskFilterName' "IMPORT_TASK_ID"

pattern ITFNName :: ImportTaskFilterName
pattern ITFNName = ImportTaskFilterName' "NAME"

pattern ITFNStatus :: ImportTaskFilterName
pattern ITFNStatus = ImportTaskFilterName' "STATUS"

{-# COMPLETE
  ITFNImportTaskId,
  ITFNName,
  ITFNStatus,
  ImportTaskFilterName' #-}

instance FromText ImportTaskFilterName where
    parser = (ImportTaskFilterName' . mk) <$> takeText

instance ToText ImportTaskFilterName where
    toText (ImportTaskFilterName' ci) = original ci

-- | Represents an enum of /known/ $ImportTaskFilterName.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ImportTaskFilterName where
    toEnum i = case i of
        0 -> ITFNImportTaskId
        1 -> ITFNName
        2 -> ITFNStatus
        _ -> (error . showText) $ "Unknown index for ImportTaskFilterName: " <> toText i
    fromEnum x = case x of
        ITFNImportTaskId -> 0
        ITFNName -> 1
        ITFNStatus -> 2
        ImportTaskFilterName' name -> (error . showText) $ "Unknown ImportTaskFilterName: " <> original name

-- | Represents the bounds of /known/ $ImportTaskFilterName.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ImportTaskFilterName where
    minBound = ITFNImportTaskId
    maxBound = ITFNStatus

instance Hashable     ImportTaskFilterName
instance NFData       ImportTaskFilterName
instance ToByteString ImportTaskFilterName
instance ToQuery      ImportTaskFilterName
instance ToHeader     ImportTaskFilterName

instance ToJSON ImportTaskFilterName where
    toJSON = toJSONText
