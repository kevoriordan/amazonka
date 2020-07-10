{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.FirelensConfigurationType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECS.Types.FirelensConfigurationType (
  FirelensConfigurationType (
    ..
    , Fluentbit
    , Fluentd
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data FirelensConfigurationType = FirelensConfigurationType' (CI
                                                               Text)
                                   deriving (Eq, Ord, Read, Show, Data,
                                             Typeable, Generic)

pattern Fluentbit :: FirelensConfigurationType
pattern Fluentbit = FirelensConfigurationType' "fluentbit"

pattern Fluentd :: FirelensConfigurationType
pattern Fluentd = FirelensConfigurationType' "fluentd"

{-# COMPLETE
  Fluentbit,
  Fluentd,
  FirelensConfigurationType' #-}

instance FromText FirelensConfigurationType where
    parser = (FirelensConfigurationType' . mk) <$> takeText

instance ToText FirelensConfigurationType where
    toText (FirelensConfigurationType' ci) = original ci

-- | Represents an enum of /known/ $FirelensConfigurationType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum FirelensConfigurationType where
    toEnum i = case i of
        0 -> Fluentbit
        1 -> Fluentd
        _ -> (error . showText) $ "Unknown index for FirelensConfigurationType: " <> toText i
    fromEnum x = case x of
        Fluentbit -> 0
        Fluentd -> 1
        FirelensConfigurationType' name -> (error . showText) $ "Unknown FirelensConfigurationType: " <> original name

-- | Represents the bounds of /known/ $FirelensConfigurationType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded FirelensConfigurationType where
    minBound = Fluentbit
    maxBound = Fluentd

instance Hashable     FirelensConfigurationType
instance NFData       FirelensConfigurationType
instance ToByteString FirelensConfigurationType
instance ToQuery      FirelensConfigurationType
instance ToHeader     FirelensConfigurationType

instance ToJSON FirelensConfigurationType where
    toJSON = toJSONText

instance FromJSON FirelensConfigurationType where
    parseJSON = parseJSONText "FirelensConfigurationType"
