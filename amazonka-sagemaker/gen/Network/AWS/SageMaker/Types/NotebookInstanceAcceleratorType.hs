{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.NotebookInstanceAcceleratorType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.NotebookInstanceAcceleratorType (
  NotebookInstanceAcceleratorType (
    ..
    , Ml_EIA1_Large
    , Ml_EIA1_Medium
    , Ml_EIA1_XLarge
    , Ml_EIA2_Large
    , Ml_EIA2_Medium
    , Ml_EIA2_XLarge
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data NotebookInstanceAcceleratorType = NotebookInstanceAcceleratorType' (CI
                                                                           Text)
                                         deriving (Eq, Ord, Read, Show, Data,
                                                   Typeable, Generic)

pattern Ml_EIA1_Large :: NotebookInstanceAcceleratorType
pattern Ml_EIA1_Large = NotebookInstanceAcceleratorType' "ml.eia1.large"

pattern Ml_EIA1_Medium :: NotebookInstanceAcceleratorType
pattern Ml_EIA1_Medium = NotebookInstanceAcceleratorType' "ml.eia1.medium"

pattern Ml_EIA1_XLarge :: NotebookInstanceAcceleratorType
pattern Ml_EIA1_XLarge = NotebookInstanceAcceleratorType' "ml.eia1.xlarge"

pattern Ml_EIA2_Large :: NotebookInstanceAcceleratorType
pattern Ml_EIA2_Large = NotebookInstanceAcceleratorType' "ml.eia2.large"

pattern Ml_EIA2_Medium :: NotebookInstanceAcceleratorType
pattern Ml_EIA2_Medium = NotebookInstanceAcceleratorType' "ml.eia2.medium"

pattern Ml_EIA2_XLarge :: NotebookInstanceAcceleratorType
pattern Ml_EIA2_XLarge = NotebookInstanceAcceleratorType' "ml.eia2.xlarge"

{-# COMPLETE
  Ml_EIA1_Large,
  Ml_EIA1_Medium,
  Ml_EIA1_XLarge,
  Ml_EIA2_Large,
  Ml_EIA2_Medium,
  Ml_EIA2_XLarge,
  NotebookInstanceAcceleratorType' #-}

instance FromText NotebookInstanceAcceleratorType where
    parser = (NotebookInstanceAcceleratorType' . mk) <$> takeText

instance ToText NotebookInstanceAcceleratorType where
    toText (NotebookInstanceAcceleratorType' ci) = original ci

-- | Represents an enum of /known/ $NotebookInstanceAcceleratorType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum NotebookInstanceAcceleratorType where
    toEnum i = case i of
        0 -> Ml_EIA1_Large
        1 -> Ml_EIA1_Medium
        2 -> Ml_EIA1_XLarge
        3 -> Ml_EIA2_Large
        4 -> Ml_EIA2_Medium
        5 -> Ml_EIA2_XLarge
        _ -> (error . showText) $ "Unknown index for NotebookInstanceAcceleratorType: " <> toText i
    fromEnum x = case x of
        Ml_EIA1_Large -> 0
        Ml_EIA1_Medium -> 1
        Ml_EIA1_XLarge -> 2
        Ml_EIA2_Large -> 3
        Ml_EIA2_Medium -> 4
        Ml_EIA2_XLarge -> 5
        NotebookInstanceAcceleratorType' name -> (error . showText) $ "Unknown NotebookInstanceAcceleratorType: " <> original name

-- | Represents the bounds of /known/ $NotebookInstanceAcceleratorType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded NotebookInstanceAcceleratorType where
    minBound = Ml_EIA1_Large
    maxBound = Ml_EIA2_XLarge

instance Hashable     NotebookInstanceAcceleratorType
instance NFData       NotebookInstanceAcceleratorType
instance ToByteString NotebookInstanceAcceleratorType
instance ToQuery      NotebookInstanceAcceleratorType
instance ToHeader     NotebookInstanceAcceleratorType

instance ToJSON NotebookInstanceAcceleratorType where
    toJSON = toJSONText

instance FromJSON NotebookInstanceAcceleratorType where
    parseJSON = parseJSONText "NotebookInstanceAcceleratorType"
