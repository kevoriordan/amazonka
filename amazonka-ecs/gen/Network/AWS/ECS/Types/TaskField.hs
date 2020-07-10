{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.TaskField
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECS.Types.TaskField (
  TaskField (
    ..
    , TFTags
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data TaskField = TaskField' (CI Text)
                   deriving (Eq, Ord, Read, Show, Data, Typeable,
                             Generic)

pattern TFTags :: TaskField
pattern TFTags = TaskField' "TAGS"

{-# COMPLETE
  TFTags,
  TaskField' #-}

instance FromText TaskField where
    parser = (TaskField' . mk) <$> takeText

instance ToText TaskField where
    toText (TaskField' ci) = original ci

-- | Represents an enum of /known/ $TaskField.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum TaskField where
    toEnum i = case i of
        0 -> TFTags
        _ -> (error . showText) $ "Unknown index for TaskField: " <> toText i
    fromEnum x = case x of
        TFTags -> 0
        TaskField' name -> (error . showText) $ "Unknown TaskField: " <> original name

-- | Represents the bounds of /known/ $TaskField.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded TaskField where
    minBound = TFTags
    maxBound = TFTags

instance Hashable     TaskField
instance NFData       TaskField
instance ToByteString TaskField
instance ToQuery      TaskField
instance ToHeader     TaskField

instance ToJSON TaskField where
    toJSON = toJSONText
