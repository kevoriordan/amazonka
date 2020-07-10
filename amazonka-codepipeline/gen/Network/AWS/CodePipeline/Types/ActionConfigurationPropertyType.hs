{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ActionConfigurationPropertyType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodePipeline.Types.ActionConfigurationPropertyType (
  ActionConfigurationPropertyType (
    ..
    , Boolean
    , Number
    , String
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ActionConfigurationPropertyType = ActionConfigurationPropertyType' (CI
                                                                           Text)
                                         deriving (Eq, Ord, Read, Show, Data,
                                                   Typeable, Generic)

pattern Boolean :: ActionConfigurationPropertyType
pattern Boolean = ActionConfigurationPropertyType' "Boolean"

pattern Number :: ActionConfigurationPropertyType
pattern Number = ActionConfigurationPropertyType' "Number"

pattern String :: ActionConfigurationPropertyType
pattern String = ActionConfigurationPropertyType' "String"

{-# COMPLETE
  Boolean,
  Number,
  String,
  ActionConfigurationPropertyType' #-}

instance FromText ActionConfigurationPropertyType where
    parser = (ActionConfigurationPropertyType' . mk) <$> takeText

instance ToText ActionConfigurationPropertyType where
    toText (ActionConfigurationPropertyType' ci) = original ci

-- | Represents an enum of /known/ $ActionConfigurationPropertyType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ActionConfigurationPropertyType where
    toEnum i = case i of
        0 -> Boolean
        1 -> Number
        2 -> String
        _ -> (error . showText) $ "Unknown index for ActionConfigurationPropertyType: " <> toText i
    fromEnum x = case x of
        Boolean -> 0
        Number -> 1
        String -> 2
        ActionConfigurationPropertyType' name -> (error . showText) $ "Unknown ActionConfigurationPropertyType: " <> original name

-- | Represents the bounds of /known/ $ActionConfigurationPropertyType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ActionConfigurationPropertyType where
    minBound = Boolean
    maxBound = String

instance Hashable     ActionConfigurationPropertyType
instance NFData       ActionConfigurationPropertyType
instance ToByteString ActionConfigurationPropertyType
instance ToQuery      ActionConfigurationPropertyType
instance ToHeader     ActionConfigurationPropertyType

instance ToJSON ActionConfigurationPropertyType where
    toJSON = toJSONText

instance FromJSON ActionConfigurationPropertyType where
    parseJSON = parseJSONText "ActionConfigurationPropertyType"
