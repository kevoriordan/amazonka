{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.CurrencyCode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DeviceFarm.Types.CurrencyCode (
  CurrencyCode (
    ..
    , Usd
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data CurrencyCode = CurrencyCode' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern Usd :: CurrencyCode
pattern Usd = CurrencyCode' "USD"

{-# COMPLETE
  Usd,
  CurrencyCode' #-}

instance FromText CurrencyCode where
    parser = (CurrencyCode' . mk) <$> takeText

instance ToText CurrencyCode where
    toText (CurrencyCode' ci) = original ci

-- | Represents an enum of /known/ $CurrencyCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum CurrencyCode where
    toEnum i = case i of
        0 -> Usd
        _ -> (error . showText) $ "Unknown index for CurrencyCode: " <> toText i
    fromEnum x = case x of
        Usd -> 0
        CurrencyCode' name -> (error . showText) $ "Unknown CurrencyCode: " <> original name

-- | Represents the bounds of /known/ $CurrencyCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded CurrencyCode where
    minBound = Usd
    maxBound = Usd

instance Hashable     CurrencyCode
instance NFData       CurrencyCode
instance ToByteString CurrencyCode
instance ToQuery      CurrencyCode
instance ToHeader     CurrencyCode

instance FromJSON CurrencyCode where
    parseJSON = parseJSONText "CurrencyCode"
