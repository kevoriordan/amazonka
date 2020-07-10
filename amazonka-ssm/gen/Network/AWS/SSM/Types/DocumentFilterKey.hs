{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.DocumentFilterKey
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.DocumentFilterKey (
  DocumentFilterKey (
    ..
    , DocumentType
    , Name
    , Owner
    , PlatformTypes
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data DocumentFilterKey = DocumentFilterKey' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern DocumentType :: DocumentFilterKey
pattern DocumentType = DocumentFilterKey' "DocumentType"

pattern Name :: DocumentFilterKey
pattern Name = DocumentFilterKey' "Name"

pattern Owner :: DocumentFilterKey
pattern Owner = DocumentFilterKey' "Owner"

pattern PlatformTypes :: DocumentFilterKey
pattern PlatformTypes = DocumentFilterKey' "PlatformTypes"

{-# COMPLETE
  DocumentType,
  Name,
  Owner,
  PlatformTypes,
  DocumentFilterKey' #-}

instance FromText DocumentFilterKey where
    parser = (DocumentFilterKey' . mk) <$> takeText

instance ToText DocumentFilterKey where
    toText (DocumentFilterKey' ci) = original ci

-- | Represents an enum of /known/ $DocumentFilterKey.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DocumentFilterKey where
    toEnum i = case i of
        0 -> DocumentType
        1 -> Name
        2 -> Owner
        3 -> PlatformTypes
        _ -> (error . showText) $ "Unknown index for DocumentFilterKey: " <> toText i
    fromEnum x = case x of
        DocumentType -> 0
        Name -> 1
        Owner -> 2
        PlatformTypes -> 3
        DocumentFilterKey' name -> (error . showText) $ "Unknown DocumentFilterKey: " <> original name

-- | Represents the bounds of /known/ $DocumentFilterKey.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DocumentFilterKey where
    minBound = DocumentType
    maxBound = PlatformTypes

instance Hashable     DocumentFilterKey
instance NFData       DocumentFilterKey
instance ToByteString DocumentFilterKey
instance ToQuery      DocumentFilterKey
instance ToHeader     DocumentFilterKey

instance ToJSON DocumentFilterKey where
    toJSON = toJSONText
