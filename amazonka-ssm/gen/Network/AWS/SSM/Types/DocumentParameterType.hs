{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.DocumentParameterType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.DocumentParameterType (
  DocumentParameterType (
    ..
    , DPTString
    , DPTStringList
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data DocumentParameterType = DocumentParameterType' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern DPTString :: DocumentParameterType
pattern DPTString = DocumentParameterType' "String"

pattern DPTStringList :: DocumentParameterType
pattern DPTStringList = DocumentParameterType' "StringList"

{-# COMPLETE
  DPTString,
  DPTStringList,
  DocumentParameterType' #-}

instance FromText DocumentParameterType where
    parser = (DocumentParameterType' . mk) <$> takeText

instance ToText DocumentParameterType where
    toText (DocumentParameterType' ci) = original ci

-- | Represents an enum of /known/ $DocumentParameterType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DocumentParameterType where
    toEnum i = case i of
        0 -> DPTString
        1 -> DPTStringList
        _ -> (error . showText) $ "Unknown index for DocumentParameterType: " <> toText i
    fromEnum x = case x of
        DPTString -> 0
        DPTStringList -> 1
        DocumentParameterType' name -> (error . showText) $ "Unknown DocumentParameterType: " <> original name

-- | Represents the bounds of /known/ $DocumentParameterType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DocumentParameterType where
    minBound = DPTString
    maxBound = DPTStringList

instance Hashable     DocumentParameterType
instance NFData       DocumentParameterType
instance ToByteString DocumentParameterType
instance ToQuery      DocumentParameterType
instance ToHeader     DocumentParameterType

instance FromJSON DocumentParameterType where
    parseJSON = parseJSONText "DocumentParameterType"
