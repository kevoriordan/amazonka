{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.BatchStrategy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.BatchStrategy (
  BatchStrategy (
    ..
    , MultiRecord
    , SingleRecord
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data BatchStrategy = BatchStrategy' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern MultiRecord :: BatchStrategy
pattern MultiRecord = BatchStrategy' "MultiRecord"

pattern SingleRecord :: BatchStrategy
pattern SingleRecord = BatchStrategy' "SingleRecord"

{-# COMPLETE
  MultiRecord,
  SingleRecord,
  BatchStrategy' #-}

instance FromText BatchStrategy where
    parser = (BatchStrategy' . mk) <$> takeText

instance ToText BatchStrategy where
    toText (BatchStrategy' ci) = original ci

-- | Represents an enum of /known/ $BatchStrategy.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum BatchStrategy where
    toEnum i = case i of
        0 -> MultiRecord
        1 -> SingleRecord
        _ -> (error . showText) $ "Unknown index for BatchStrategy: " <> toText i
    fromEnum x = case x of
        MultiRecord -> 0
        SingleRecord -> 1
        BatchStrategy' name -> (error . showText) $ "Unknown BatchStrategy: " <> original name

-- | Represents the bounds of /known/ $BatchStrategy.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded BatchStrategy where
    minBound = MultiRecord
    maxBound = SingleRecord

instance Hashable     BatchStrategy
instance NFData       BatchStrategy
instance ToByteString BatchStrategy
instance ToQuery      BatchStrategy
instance ToHeader     BatchStrategy

instance ToJSON BatchStrategy where
    toJSON = toJSONText

instance FromJSON BatchStrategy where
    parseJSON = parseJSONText "BatchStrategy"
