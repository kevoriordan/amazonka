{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.PropagateTags
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECS.Types.PropagateTags (
  PropagateTags (
    ..
    , PTService
    , PTTaskDefinition
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data PropagateTags = PropagateTags' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern PTService :: PropagateTags
pattern PTService = PropagateTags' "SERVICE"

pattern PTTaskDefinition :: PropagateTags
pattern PTTaskDefinition = PropagateTags' "TASK_DEFINITION"

{-# COMPLETE
  PTService,
  PTTaskDefinition,
  PropagateTags' #-}

instance FromText PropagateTags where
    parser = (PropagateTags' . mk) <$> takeText

instance ToText PropagateTags where
    toText (PropagateTags' ci) = original ci

-- | Represents an enum of /known/ $PropagateTags.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum PropagateTags where
    toEnum i = case i of
        0 -> PTService
        1 -> PTTaskDefinition
        _ -> (error . showText) $ "Unknown index for PropagateTags: " <> toText i
    fromEnum x = case x of
        PTService -> 0
        PTTaskDefinition -> 1
        PropagateTags' name -> (error . showText) $ "Unknown PropagateTags: " <> original name

-- | Represents the bounds of /known/ $PropagateTags.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded PropagateTags where
    minBound = PTService
    maxBound = PTTaskDefinition

instance Hashable     PropagateTags
instance NFData       PropagateTags
instance ToByteString PropagateTags
instance ToQuery      PropagateTags
instance ToHeader     PropagateTags

instance ToJSON PropagateTags where
    toJSON = toJSONText

instance FromJSON PropagateTags where
    parseJSON = parseJSONText "PropagateTags"
