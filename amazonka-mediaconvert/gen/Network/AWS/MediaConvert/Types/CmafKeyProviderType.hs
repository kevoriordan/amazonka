{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.CmafKeyProviderType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.CmafKeyProviderType (
  CmafKeyProviderType (
    ..
    , Speke
    , StaticKey
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Specify whether your DRM encryption key is static or from a key provider that follows the SPEKE standard. For more information about SPEKE, see https://docs.aws.amazon.com/speke/latest/documentation/what-is-speke.html.
data CmafKeyProviderType = CmafKeyProviderType' (CI
                                                   Text)
                             deriving (Eq, Ord, Read, Show, Data, Typeable,
                                       Generic)

pattern Speke :: CmafKeyProviderType
pattern Speke = CmafKeyProviderType' "SPEKE"

pattern StaticKey :: CmafKeyProviderType
pattern StaticKey = CmafKeyProviderType' "STATIC_KEY"

{-# COMPLETE
  Speke,
  StaticKey,
  CmafKeyProviderType' #-}

instance FromText CmafKeyProviderType where
    parser = (CmafKeyProviderType' . mk) <$> takeText

instance ToText CmafKeyProviderType where
    toText (CmafKeyProviderType' ci) = original ci

-- | Represents an enum of /known/ $CmafKeyProviderType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum CmafKeyProviderType where
    toEnum i = case i of
        0 -> Speke
        1 -> StaticKey
        _ -> (error . showText) $ "Unknown index for CmafKeyProviderType: " <> toText i
    fromEnum x = case x of
        Speke -> 0
        StaticKey -> 1
        CmafKeyProviderType' name -> (error . showText) $ "Unknown CmafKeyProviderType: " <> original name

-- | Represents the bounds of /known/ $CmafKeyProviderType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded CmafKeyProviderType where
    minBound = Speke
    maxBound = StaticKey

instance Hashable     CmafKeyProviderType
instance NFData       CmafKeyProviderType
instance ToByteString CmafKeyProviderType
instance ToQuery      CmafKeyProviderType
instance ToHeader     CmafKeyProviderType

instance ToJSON CmafKeyProviderType where
    toJSON = toJSONText

instance FromJSON CmafKeyProviderType where
    parseJSON = parseJSONText "CmafKeyProviderType"
