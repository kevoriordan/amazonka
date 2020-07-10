{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Types.WrappingKeySpec
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KMS.Types.WrappingKeySpec (
  WrappingKeySpec (
    ..
    , Rsa2048
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data WrappingKeySpec = WrappingKeySpec' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern Rsa2048 :: WrappingKeySpec
pattern Rsa2048 = WrappingKeySpec' "RSA_2048"

{-# COMPLETE
  Rsa2048,
  WrappingKeySpec' #-}

instance FromText WrappingKeySpec where
    parser = (WrappingKeySpec' . mk) <$> takeText

instance ToText WrappingKeySpec where
    toText (WrappingKeySpec' ci) = original ci

-- | Represents an enum of /known/ $WrappingKeySpec.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum WrappingKeySpec where
    toEnum i = case i of
        0 -> Rsa2048
        _ -> (error . showText) $ "Unknown index for WrappingKeySpec: " <> toText i
    fromEnum x = case x of
        Rsa2048 -> 0
        WrappingKeySpec' name -> (error . showText) $ "Unknown WrappingKeySpec: " <> original name

-- | Represents the bounds of /known/ $WrappingKeySpec.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded WrappingKeySpec where
    minBound = Rsa2048
    maxBound = Rsa2048

instance Hashable     WrappingKeySpec
instance NFData       WrappingKeySpec
instance ToByteString WrappingKeySpec
instance ToQuery      WrappingKeySpec
instance ToHeader     WrappingKeySpec

instance ToJSON WrappingKeySpec where
    toJSON = toJSONText
