{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideo.Types.UpdateDataRetentionOperation
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KinesisVideo.Types.UpdateDataRetentionOperation (
  UpdateDataRetentionOperation (
    ..
    , DecreaseDataRetention
    , IncreaseDataRetention
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data UpdateDataRetentionOperation = UpdateDataRetentionOperation' (CI
                                                                     Text)
                                      deriving (Eq, Ord, Read, Show, Data,
                                                Typeable, Generic)

pattern DecreaseDataRetention :: UpdateDataRetentionOperation
pattern DecreaseDataRetention = UpdateDataRetentionOperation' "DECREASE_DATA_RETENTION"

pattern IncreaseDataRetention :: UpdateDataRetentionOperation
pattern IncreaseDataRetention = UpdateDataRetentionOperation' "INCREASE_DATA_RETENTION"

{-# COMPLETE
  DecreaseDataRetention,
  IncreaseDataRetention,
  UpdateDataRetentionOperation' #-}

instance FromText UpdateDataRetentionOperation where
    parser = (UpdateDataRetentionOperation' . mk) <$> takeText

instance ToText UpdateDataRetentionOperation where
    toText (UpdateDataRetentionOperation' ci) = original ci

-- | Represents an enum of /known/ $UpdateDataRetentionOperation.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum UpdateDataRetentionOperation where
    toEnum i = case i of
        0 -> DecreaseDataRetention
        1 -> IncreaseDataRetention
        _ -> (error . showText) $ "Unknown index for UpdateDataRetentionOperation: " <> toText i
    fromEnum x = case x of
        DecreaseDataRetention -> 0
        IncreaseDataRetention -> 1
        UpdateDataRetentionOperation' name -> (error . showText) $ "Unknown UpdateDataRetentionOperation: " <> original name

-- | Represents the bounds of /known/ $UpdateDataRetentionOperation.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded UpdateDataRetentionOperation where
    minBound = DecreaseDataRetention
    maxBound = IncreaseDataRetention

instance Hashable     UpdateDataRetentionOperation
instance NFData       UpdateDataRetentionOperation
instance ToByteString UpdateDataRetentionOperation
instance ToQuery      UpdateDataRetentionOperation
instance ToHeader     UpdateDataRetentionOperation

instance ToJSON UpdateDataRetentionOperation where
    toJSON = toJSONText
