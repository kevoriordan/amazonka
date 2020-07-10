{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.ConfigurationOptionValueType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticBeanstalk.Types.ConfigurationOptionValueType (
  ConfigurationOptionValueType (
    ..
    , List
    , Scalar
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ConfigurationOptionValueType = ConfigurationOptionValueType' (CI
                                                                     Text)
                                      deriving (Eq, Ord, Read, Show, Data,
                                                Typeable, Generic)

pattern List :: ConfigurationOptionValueType
pattern List = ConfigurationOptionValueType' "List"

pattern Scalar :: ConfigurationOptionValueType
pattern Scalar = ConfigurationOptionValueType' "Scalar"

{-# COMPLETE
  List,
  Scalar,
  ConfigurationOptionValueType' #-}

instance FromText ConfigurationOptionValueType where
    parser = (ConfigurationOptionValueType' . mk) <$> takeText

instance ToText ConfigurationOptionValueType where
    toText (ConfigurationOptionValueType' ci) = original ci

-- | Represents an enum of /known/ $ConfigurationOptionValueType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ConfigurationOptionValueType where
    toEnum i = case i of
        0 -> List
        1 -> Scalar
        _ -> (error . showText) $ "Unknown index for ConfigurationOptionValueType: " <> toText i
    fromEnum x = case x of
        List -> 0
        Scalar -> 1
        ConfigurationOptionValueType' name -> (error . showText) $ "Unknown ConfigurationOptionValueType: " <> original name

-- | Represents the bounds of /known/ $ConfigurationOptionValueType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ConfigurationOptionValueType where
    minBound = List
    maxBound = Scalar

instance Hashable     ConfigurationOptionValueType
instance NFData       ConfigurationOptionValueType
instance ToByteString ConfigurationOptionValueType
instance ToQuery      ConfigurationOptionValueType
instance ToHeader     ConfigurationOptionValueType

instance FromXML ConfigurationOptionValueType where
    parseXML = parseXMLText "ConfigurationOptionValueType"
