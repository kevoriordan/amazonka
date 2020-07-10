{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.ProjectStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Rekognition.Types.ProjectStatus (
  ProjectStatus (
    ..
    , PSCreated
    , PSCreating
    , PSDeleting
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ProjectStatus = ProjectStatus' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern PSCreated :: ProjectStatus
pattern PSCreated = ProjectStatus' "CREATED"

pattern PSCreating :: ProjectStatus
pattern PSCreating = ProjectStatus' "CREATING"

pattern PSDeleting :: ProjectStatus
pattern PSDeleting = ProjectStatus' "DELETING"

{-# COMPLETE
  PSCreated,
  PSCreating,
  PSDeleting,
  ProjectStatus' #-}

instance FromText ProjectStatus where
    parser = (ProjectStatus' . mk) <$> takeText

instance ToText ProjectStatus where
    toText (ProjectStatus' ci) = original ci

-- | Represents an enum of /known/ $ProjectStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ProjectStatus where
    toEnum i = case i of
        0 -> PSCreated
        1 -> PSCreating
        2 -> PSDeleting
        _ -> (error . showText) $ "Unknown index for ProjectStatus: " <> toText i
    fromEnum x = case x of
        PSCreated -> 0
        PSCreating -> 1
        PSDeleting -> 2
        ProjectStatus' name -> (error . showText) $ "Unknown ProjectStatus: " <> original name

-- | Represents the bounds of /known/ $ProjectStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ProjectStatus where
    minBound = PSCreated
    maxBound = PSDeleting

instance Hashable     ProjectStatus
instance NFData       ProjectStatus
instance ToByteString ProjectStatus
instance ToQuery      ProjectStatus
instance ToHeader     ProjectStatus

instance FromJSON ProjectStatus where
    parseJSON = parseJSONText "ProjectStatus"
