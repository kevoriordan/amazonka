{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Types.ExpirationModelType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KMS.Types.ExpirationModelType (
  ExpirationModelType (
    ..
    , KeyMaterialDoesNotExpire
    , KeyMaterialExpires
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ExpirationModelType = ExpirationModelType' (CI
                                                   Text)
                             deriving (Eq, Ord, Read, Show, Data, Typeable,
                                       Generic)

pattern KeyMaterialDoesNotExpire :: ExpirationModelType
pattern KeyMaterialDoesNotExpire = ExpirationModelType' "KEY_MATERIAL_DOES_NOT_EXPIRE"

pattern KeyMaterialExpires :: ExpirationModelType
pattern KeyMaterialExpires = ExpirationModelType' "KEY_MATERIAL_EXPIRES"

{-# COMPLETE
  KeyMaterialDoesNotExpire,
  KeyMaterialExpires,
  ExpirationModelType' #-}

instance FromText ExpirationModelType where
    parser = (ExpirationModelType' . mk) <$> takeText

instance ToText ExpirationModelType where
    toText (ExpirationModelType' ci) = original ci

-- | Represents an enum of /known/ $ExpirationModelType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ExpirationModelType where
    toEnum i = case i of
        0 -> KeyMaterialDoesNotExpire
        1 -> KeyMaterialExpires
        _ -> (error . showText) $ "Unknown index for ExpirationModelType: " <> toText i
    fromEnum x = case x of
        KeyMaterialDoesNotExpire -> 0
        KeyMaterialExpires -> 1
        ExpirationModelType' name -> (error . showText) $ "Unknown ExpirationModelType: " <> original name

-- | Represents the bounds of /known/ $ExpirationModelType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ExpirationModelType where
    minBound = KeyMaterialDoesNotExpire
    maxBound = KeyMaterialExpires

instance Hashable     ExpirationModelType
instance NFData       ExpirationModelType
instance ToByteString ExpirationModelType
instance ToQuery      ExpirationModelType
instance ToHeader     ExpirationModelType

instance ToJSON ExpirationModelType where
    toJSON = toJSONText

instance FromJSON ExpirationModelType where
    parseJSON = parseJSONText "ExpirationModelType"
