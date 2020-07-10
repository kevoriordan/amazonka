{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.DmsSSLModeValue
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DMS.Types.DmsSSLModeValue (
  DmsSSLModeValue (
    ..
    , DSMVNone
    , DSMVRequire
    , DSMVVerifyCa
    , DSMVVerifyFull
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data DmsSSLModeValue = DmsSSLModeValue' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern DSMVNone :: DmsSSLModeValue
pattern DSMVNone = DmsSSLModeValue' "none"

pattern DSMVRequire :: DmsSSLModeValue
pattern DSMVRequire = DmsSSLModeValue' "require"

pattern DSMVVerifyCa :: DmsSSLModeValue
pattern DSMVVerifyCa = DmsSSLModeValue' "verify-ca"

pattern DSMVVerifyFull :: DmsSSLModeValue
pattern DSMVVerifyFull = DmsSSLModeValue' "verify-full"

{-# COMPLETE
  DSMVNone,
  DSMVRequire,
  DSMVVerifyCa,
  DSMVVerifyFull,
  DmsSSLModeValue' #-}

instance FromText DmsSSLModeValue where
    parser = (DmsSSLModeValue' . mk) <$> takeText

instance ToText DmsSSLModeValue where
    toText (DmsSSLModeValue' ci) = original ci

-- | Represents an enum of /known/ $DmsSSLModeValue.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DmsSSLModeValue where
    toEnum i = case i of
        0 -> DSMVNone
        1 -> DSMVRequire
        2 -> DSMVVerifyCa
        3 -> DSMVVerifyFull
        _ -> (error . showText) $ "Unknown index for DmsSSLModeValue: " <> toText i
    fromEnum x = case x of
        DSMVNone -> 0
        DSMVRequire -> 1
        DSMVVerifyCa -> 2
        DSMVVerifyFull -> 3
        DmsSSLModeValue' name -> (error . showText) $ "Unknown DmsSSLModeValue: " <> original name

-- | Represents the bounds of /known/ $DmsSSLModeValue.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DmsSSLModeValue where
    minBound = DSMVNone
    maxBound = DSMVVerifyFull

instance Hashable     DmsSSLModeValue
instance NFData       DmsSSLModeValue
instance ToByteString DmsSSLModeValue
instance ToQuery      DmsSSLModeValue
instance ToHeader     DmsSSLModeValue

instance ToJSON DmsSSLModeValue where
    toJSON = toJSONText

instance FromJSON DmsSSLModeValue where
    parseJSON = parseJSONText "DmsSSLModeValue"
