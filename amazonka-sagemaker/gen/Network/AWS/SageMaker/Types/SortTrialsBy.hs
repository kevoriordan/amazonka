{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.SortTrialsBy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.SortTrialsBy (
  SortTrialsBy (
    ..
    , STBCreationTime
    , STBName
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data SortTrialsBy = SortTrialsBy' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern STBCreationTime :: SortTrialsBy
pattern STBCreationTime = SortTrialsBy' "CreationTime"

pattern STBName :: SortTrialsBy
pattern STBName = SortTrialsBy' "Name"

{-# COMPLETE
  STBCreationTime,
  STBName,
  SortTrialsBy' #-}

instance FromText SortTrialsBy where
    parser = (SortTrialsBy' . mk) <$> takeText

instance ToText SortTrialsBy where
    toText (SortTrialsBy' ci) = original ci

-- | Represents an enum of /known/ $SortTrialsBy.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum SortTrialsBy where
    toEnum i = case i of
        0 -> STBCreationTime
        1 -> STBName
        _ -> (error . showText) $ "Unknown index for SortTrialsBy: " <> toText i
    fromEnum x = case x of
        STBCreationTime -> 0
        STBName -> 1
        SortTrialsBy' name -> (error . showText) $ "Unknown SortTrialsBy: " <> original name

-- | Represents the bounds of /known/ $SortTrialsBy.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded SortTrialsBy where
    minBound = STBCreationTime
    maxBound = STBName

instance Hashable     SortTrialsBy
instance NFData       SortTrialsBy
instance ToByteString SortTrialsBy
instance ToQuery      SortTrialsBy
instance ToHeader     SortTrialsBy

instance ToJSON SortTrialsBy where
    toJSON = toJSONText
