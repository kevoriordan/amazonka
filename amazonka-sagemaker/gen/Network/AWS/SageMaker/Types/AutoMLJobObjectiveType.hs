{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AutoMLJobObjectiveType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.AutoMLJobObjectiveType (
  AutoMLJobObjectiveType (
    ..
    , AMLJOTMaximize
    , AMLJOTMinimize
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AutoMLJobObjectiveType = AutoMLJobObjectiveType' (CI
                                                         Text)
                                deriving (Eq, Ord, Read, Show, Data, Typeable,
                                          Generic)

pattern AMLJOTMaximize :: AutoMLJobObjectiveType
pattern AMLJOTMaximize = AutoMLJobObjectiveType' "Maximize"

pattern AMLJOTMinimize :: AutoMLJobObjectiveType
pattern AMLJOTMinimize = AutoMLJobObjectiveType' "Minimize"

{-# COMPLETE
  AMLJOTMaximize,
  AMLJOTMinimize,
  AutoMLJobObjectiveType' #-}

instance FromText AutoMLJobObjectiveType where
    parser = (AutoMLJobObjectiveType' . mk) <$> takeText

instance ToText AutoMLJobObjectiveType where
    toText (AutoMLJobObjectiveType' ci) = original ci

-- | Represents an enum of /known/ $AutoMLJobObjectiveType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AutoMLJobObjectiveType where
    toEnum i = case i of
        0 -> AMLJOTMaximize
        1 -> AMLJOTMinimize
        _ -> (error . showText) $ "Unknown index for AutoMLJobObjectiveType: " <> toText i
    fromEnum x = case x of
        AMLJOTMaximize -> 0
        AMLJOTMinimize -> 1
        AutoMLJobObjectiveType' name -> (error . showText) $ "Unknown AutoMLJobObjectiveType: " <> original name

-- | Represents the bounds of /known/ $AutoMLJobObjectiveType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AutoMLJobObjectiveType where
    minBound = AMLJOTMaximize
    maxBound = AMLJOTMinimize

instance Hashable     AutoMLJobObjectiveType
instance NFData       AutoMLJobObjectiveType
instance ToByteString AutoMLJobObjectiveType
instance ToQuery      AutoMLJobObjectiveType
instance ToHeader     AutoMLJobObjectiveType

instance FromJSON AutoMLJobObjectiveType where
    parseJSON = parseJSONText "AutoMLJobObjectiveType"
