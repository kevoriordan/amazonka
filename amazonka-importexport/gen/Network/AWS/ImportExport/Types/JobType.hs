{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ImportExport.Types.JobType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ImportExport.Types.JobType (
  JobType (
    ..
    , Export
    , Import
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Specifies whether the job to initiate is an import or export job.
data JobType = JobType' (CI Text)
                 deriving (Eq, Ord, Read, Show, Data, Typeable,
                           Generic)

pattern Export :: JobType
pattern Export = JobType' "Export"

pattern Import :: JobType
pattern Import = JobType' "Import"

{-# COMPLETE
  Export,
  Import,
  JobType' #-}

instance FromText JobType where
    parser = (JobType' . mk) <$> takeText

instance ToText JobType where
    toText (JobType' ci) = original ci

-- | Represents an enum of /known/ $JobType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum JobType where
    toEnum i = case i of
        0 -> Export
        1 -> Import
        _ -> (error . showText) $ "Unknown index for JobType: " <> toText i
    fromEnum x = case x of
        Export -> 0
        Import -> 1
        JobType' name -> (error . showText) $ "Unknown JobType: " <> original name

-- | Represents the bounds of /known/ $JobType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded JobType where
    minBound = Export
    maxBound = Import

instance Hashable     JobType
instance NFData       JobType
instance ToByteString JobType
instance ToQuery      JobType
instance ToHeader     JobType

instance FromXML JobType where
    parseXML = parseXMLText "JobType"
