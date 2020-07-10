{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.SortTrialComponentsBy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.SortTrialComponentsBy (
  SortTrialComponentsBy (
    ..
    , STCBCreationTime
    , STCBName
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data SortTrialComponentsBy = SortTrialComponentsBy' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern STCBCreationTime :: SortTrialComponentsBy
pattern STCBCreationTime = SortTrialComponentsBy' "CreationTime"

pattern STCBName :: SortTrialComponentsBy
pattern STCBName = SortTrialComponentsBy' "Name"

{-# COMPLETE
  STCBCreationTime,
  STCBName,
  SortTrialComponentsBy' #-}

instance FromText SortTrialComponentsBy where
    parser = (SortTrialComponentsBy' . mk) <$> takeText

instance ToText SortTrialComponentsBy where
    toText (SortTrialComponentsBy' ci) = original ci

-- | Represents an enum of /known/ $SortTrialComponentsBy.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum SortTrialComponentsBy where
    toEnum i = case i of
        0 -> STCBCreationTime
        1 -> STCBName
        _ -> (error . showText) $ "Unknown index for SortTrialComponentsBy: " <> toText i
    fromEnum x = case x of
        STCBCreationTime -> 0
        STCBName -> 1
        SortTrialComponentsBy' name -> (error . showText) $ "Unknown SortTrialComponentsBy: " <> original name

-- | Represents the bounds of /known/ $SortTrialComponentsBy.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded SortTrialComponentsBy where
    minBound = STCBCreationTime
    maxBound = STCBName

instance Hashable     SortTrialComponentsBy
instance NFData       SortTrialComponentsBy
instance ToByteString SortTrialComponentsBy
instance ToQuery      SortTrialComponentsBy
instance ToHeader     SortTrialComponentsBy

instance ToJSON SortTrialComponentsBy where
    toJSON = toJSONText
