{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.PhoneNumberType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Connect.Types.PhoneNumberType (
  PhoneNumberType (
    ..
    , Did
    , TollFree
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data PhoneNumberType = PhoneNumberType' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern Did :: PhoneNumberType
pattern Did = PhoneNumberType' "DID"

pattern TollFree :: PhoneNumberType
pattern TollFree = PhoneNumberType' "TOLL_FREE"

{-# COMPLETE
  Did,
  TollFree,
  PhoneNumberType' #-}

instance FromText PhoneNumberType where
    parser = (PhoneNumberType' . mk) <$> takeText

instance ToText PhoneNumberType where
    toText (PhoneNumberType' ci) = original ci

-- | Represents an enum of /known/ $PhoneNumberType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum PhoneNumberType where
    toEnum i = case i of
        0 -> Did
        1 -> TollFree
        _ -> (error . showText) $ "Unknown index for PhoneNumberType: " <> toText i
    fromEnum x = case x of
        Did -> 0
        TollFree -> 1
        PhoneNumberType' name -> (error . showText) $ "Unknown PhoneNumberType: " <> original name

-- | Represents the bounds of /known/ $PhoneNumberType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded PhoneNumberType where
    minBound = Did
    maxBound = TollFree

instance Hashable     PhoneNumberType
instance NFData       PhoneNumberType
instance ToByteString PhoneNumberType
instance ToQuery      PhoneNumberType
instance ToHeader     PhoneNumberType

instance ToJSON PhoneNumberType where
    toJSON = toJSONText

instance FromJSON PhoneNumberType where
    parseJSON = parseJSONText "PhoneNumberType"
