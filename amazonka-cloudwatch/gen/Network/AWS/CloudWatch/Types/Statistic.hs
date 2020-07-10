{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types.Statistic
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudWatch.Types.Statistic (
  Statistic (
    ..
    , Average
    , Maximum
    , Minimum
    , SampleCount
    , Sum
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data Statistic = Statistic' (CI Text)
                   deriving (Eq, Ord, Read, Show, Data, Typeable,
                             Generic)

pattern Average :: Statistic
pattern Average = Statistic' "Average"

pattern Maximum :: Statistic
pattern Maximum = Statistic' "Maximum"

pattern Minimum :: Statistic
pattern Minimum = Statistic' "Minimum"

pattern SampleCount :: Statistic
pattern SampleCount = Statistic' "SampleCount"

pattern Sum :: Statistic
pattern Sum = Statistic' "Sum"

{-# COMPLETE
  Average,
  Maximum,
  Minimum,
  SampleCount,
  Sum,
  Statistic' #-}

instance FromText Statistic where
    parser = (Statistic' . mk) <$> takeText

instance ToText Statistic where
    toText (Statistic' ci) = original ci

-- | Represents an enum of /known/ $Statistic.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Statistic where
    toEnum i = case i of
        0 -> Average
        1 -> Maximum
        2 -> Minimum
        3 -> SampleCount
        4 -> Sum
        _ -> (error . showText) $ "Unknown index for Statistic: " <> toText i
    fromEnum x = case x of
        Average -> 0
        Maximum -> 1
        Minimum -> 2
        SampleCount -> 3
        Sum -> 4
        Statistic' name -> (error . showText) $ "Unknown Statistic: " <> original name

-- | Represents the bounds of /known/ $Statistic.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Statistic where
    minBound = Average
    maxBound = Sum

instance Hashable     Statistic
instance NFData       Statistic
instance ToByteString Statistic
instance ToQuery      Statistic
instance ToHeader     Statistic

instance FromXML Statistic where
    parseXML = parseXMLText "Statistic"
