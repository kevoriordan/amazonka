{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.TaskDefinitionField
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECS.Types.TaskDefinitionField (
  TaskDefinitionField (
    ..
    , TDFTags
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data TaskDefinitionField = TaskDefinitionField' (CI
                                                   Text)
                             deriving (Eq, Ord, Read, Show, Data, Typeable,
                                       Generic)

pattern TDFTags :: TaskDefinitionField
pattern TDFTags = TaskDefinitionField' "TAGS"

{-# COMPLETE
  TDFTags,
  TaskDefinitionField' #-}

instance FromText TaskDefinitionField where
    parser = (TaskDefinitionField' . mk) <$> takeText

instance ToText TaskDefinitionField where
    toText (TaskDefinitionField' ci) = original ci

-- | Represents an enum of /known/ $TaskDefinitionField.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum TaskDefinitionField where
    toEnum i = case i of
        0 -> TDFTags
        _ -> (error . showText) $ "Unknown index for TaskDefinitionField: " <> toText i
    fromEnum x = case x of
        TDFTags -> 0
        TaskDefinitionField' name -> (error . showText) $ "Unknown TaskDefinitionField: " <> original name

-- | Represents the bounds of /known/ $TaskDefinitionField.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded TaskDefinitionField where
    minBound = TDFTags
    maxBound = TDFTags

instance Hashable     TaskDefinitionField
instance NFData       TaskDefinitionField
instance ToByteString TaskDefinitionField
instance ToQuery      TaskDefinitionField
instance ToHeader     TaskDefinitionField

instance ToJSON TaskDefinitionField where
    toJSON = toJSONText
