{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Types.KeyManagerType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KMS.Types.KeyManagerType (
  KeyManagerType (
    ..
    , AWS
    , Customer
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data KeyManagerType = KeyManagerType' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern AWS :: KeyManagerType
pattern AWS = KeyManagerType' "AWS"

pattern Customer :: KeyManagerType
pattern Customer = KeyManagerType' "CUSTOMER"

{-# COMPLETE
  AWS,
  Customer,
  KeyManagerType' #-}

instance FromText KeyManagerType where
    parser = (KeyManagerType' . mk) <$> takeText

instance ToText KeyManagerType where
    toText (KeyManagerType' ci) = original ci

-- | Represents an enum of /known/ $KeyManagerType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum KeyManagerType where
    toEnum i = case i of
        0 -> AWS
        1 -> Customer
        _ -> (error . showText) $ "Unknown index for KeyManagerType: " <> toText i
    fromEnum x = case x of
        AWS -> 0
        Customer -> 1
        KeyManagerType' name -> (error . showText) $ "Unknown KeyManagerType: " <> original name

-- | Represents the bounds of /known/ $KeyManagerType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded KeyManagerType where
    minBound = AWS
    maxBound = Customer

instance Hashable     KeyManagerType
instance NFData       KeyManagerType
instance ToByteString KeyManagerType
instance ToQuery      KeyManagerType
instance ToHeader     KeyManagerType

instance FromJSON KeyManagerType where
    parseJSON = parseJSONText "KeyManagerType"
