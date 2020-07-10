{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.HyperParameterTuningJobSortByOptions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.HyperParameterTuningJobSortByOptions (
  HyperParameterTuningJobSortByOptions (
    ..
    , HPTJSBOCreationTime
    , HPTJSBOName
    , HPTJSBOStatus
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data HyperParameterTuningJobSortByOptions = HyperParameterTuningJobSortByOptions' (CI
                                                                                     Text)
                                              deriving (Eq, Ord, Read, Show,
                                                        Data, Typeable, Generic)

pattern HPTJSBOCreationTime :: HyperParameterTuningJobSortByOptions
pattern HPTJSBOCreationTime = HyperParameterTuningJobSortByOptions' "CreationTime"

pattern HPTJSBOName :: HyperParameterTuningJobSortByOptions
pattern HPTJSBOName = HyperParameterTuningJobSortByOptions' "Name"

pattern HPTJSBOStatus :: HyperParameterTuningJobSortByOptions
pattern HPTJSBOStatus = HyperParameterTuningJobSortByOptions' "Status"

{-# COMPLETE
  HPTJSBOCreationTime,
  HPTJSBOName,
  HPTJSBOStatus,
  HyperParameterTuningJobSortByOptions' #-}

instance FromText HyperParameterTuningJobSortByOptions where
    parser = (HyperParameterTuningJobSortByOptions' . mk) <$> takeText

instance ToText HyperParameterTuningJobSortByOptions where
    toText (HyperParameterTuningJobSortByOptions' ci) = original ci

-- | Represents an enum of /known/ $HyperParameterTuningJobSortByOptions.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum HyperParameterTuningJobSortByOptions where
    toEnum i = case i of
        0 -> HPTJSBOCreationTime
        1 -> HPTJSBOName
        2 -> HPTJSBOStatus
        _ -> (error . showText) $ "Unknown index for HyperParameterTuningJobSortByOptions: " <> toText i
    fromEnum x = case x of
        HPTJSBOCreationTime -> 0
        HPTJSBOName -> 1
        HPTJSBOStatus -> 2
        HyperParameterTuningJobSortByOptions' name -> (error . showText) $ "Unknown HyperParameterTuningJobSortByOptions: " <> original name

-- | Represents the bounds of /known/ $HyperParameterTuningJobSortByOptions.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded HyperParameterTuningJobSortByOptions where
    minBound = HPTJSBOCreationTime
    maxBound = HPTJSBOStatus

instance Hashable     HyperParameterTuningJobSortByOptions
instance NFData       HyperParameterTuningJobSortByOptions
instance ToByteString HyperParameterTuningJobSortByOptions
instance ToQuery      HyperParameterTuningJobSortByOptions
instance ToHeader     HyperParameterTuningJobSortByOptions

instance ToJSON HyperParameterTuningJobSortByOptions where
    toJSON = toJSONText
