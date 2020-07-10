{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.JobBookmarksEncryptionMode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.JobBookmarksEncryptionMode (
  JobBookmarksEncryptionMode (
    ..
    , JBEMCseKMS
    , JBEMDisabled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data JobBookmarksEncryptionMode = JobBookmarksEncryptionMode' (CI
                                                                 Text)
                                    deriving (Eq, Ord, Read, Show, Data,
                                              Typeable, Generic)

pattern JBEMCseKMS :: JobBookmarksEncryptionMode
pattern JBEMCseKMS = JobBookmarksEncryptionMode' "CSE-KMS"

pattern JBEMDisabled :: JobBookmarksEncryptionMode
pattern JBEMDisabled = JobBookmarksEncryptionMode' "DISABLED"

{-# COMPLETE
  JBEMCseKMS,
  JBEMDisabled,
  JobBookmarksEncryptionMode' #-}

instance FromText JobBookmarksEncryptionMode where
    parser = (JobBookmarksEncryptionMode' . mk) <$> takeText

instance ToText JobBookmarksEncryptionMode where
    toText (JobBookmarksEncryptionMode' ci) = original ci

-- | Represents an enum of /known/ $JobBookmarksEncryptionMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum JobBookmarksEncryptionMode where
    toEnum i = case i of
        0 -> JBEMCseKMS
        1 -> JBEMDisabled
        _ -> (error . showText) $ "Unknown index for JobBookmarksEncryptionMode: " <> toText i
    fromEnum x = case x of
        JBEMCseKMS -> 0
        JBEMDisabled -> 1
        JobBookmarksEncryptionMode' name -> (error . showText) $ "Unknown JobBookmarksEncryptionMode: " <> original name

-- | Represents the bounds of /known/ $JobBookmarksEncryptionMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded JobBookmarksEncryptionMode where
    minBound = JBEMCseKMS
    maxBound = JBEMDisabled

instance Hashable     JobBookmarksEncryptionMode
instance NFData       JobBookmarksEncryptionMode
instance ToByteString JobBookmarksEncryptionMode
instance ToQuery      JobBookmarksEncryptionMode
instance ToHeader     JobBookmarksEncryptionMode

instance ToJSON JobBookmarksEncryptionMode where
    toJSON = toJSONText

instance FromJSON JobBookmarksEncryptionMode where
    parseJSON = parseJSONText "JobBookmarksEncryptionMode"
