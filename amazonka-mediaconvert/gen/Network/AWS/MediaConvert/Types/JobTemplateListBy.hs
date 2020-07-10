{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.JobTemplateListBy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.JobTemplateListBy (
  JobTemplateListBy (
    ..
    , JTLBCreationDate
    , JTLBName
    , JTLBSystem
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Optional. When you request a list of job templates, you can choose to list them alphabetically by NAME or chronologically by CREATION_DATE. If you don't specify, the service will list them by name.
data JobTemplateListBy = JobTemplateListBy' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern JTLBCreationDate :: JobTemplateListBy
pattern JTLBCreationDate = JobTemplateListBy' "CREATION_DATE"

pattern JTLBName :: JobTemplateListBy
pattern JTLBName = JobTemplateListBy' "NAME"

pattern JTLBSystem :: JobTemplateListBy
pattern JTLBSystem = JobTemplateListBy' "SYSTEM"

{-# COMPLETE
  JTLBCreationDate,
  JTLBName,
  JTLBSystem,
  JobTemplateListBy' #-}

instance FromText JobTemplateListBy where
    parser = (JobTemplateListBy' . mk) <$> takeText

instance ToText JobTemplateListBy where
    toText (JobTemplateListBy' ci) = original ci

-- | Represents an enum of /known/ $JobTemplateListBy.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum JobTemplateListBy where
    toEnum i = case i of
        0 -> JTLBCreationDate
        1 -> JTLBName
        2 -> JTLBSystem
        _ -> (error . showText) $ "Unknown index for JobTemplateListBy: " <> toText i
    fromEnum x = case x of
        JTLBCreationDate -> 0
        JTLBName -> 1
        JTLBSystem -> 2
        JobTemplateListBy' name -> (error . showText) $ "Unknown JobTemplateListBy: " <> original name

-- | Represents the bounds of /known/ $JobTemplateListBy.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded JobTemplateListBy where
    minBound = JTLBCreationDate
    maxBound = JTLBSystem

instance Hashable     JobTemplateListBy
instance NFData       JobTemplateListBy
instance ToByteString JobTemplateListBy
instance ToQuery      JobTemplateListBy
instance ToHeader     JobTemplateListBy

instance ToJSON JobTemplateListBy where
    toJSON = toJSONText
