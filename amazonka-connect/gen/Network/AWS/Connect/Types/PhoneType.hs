{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.PhoneType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Connect.Types.PhoneType (
  PhoneType (
    ..
    , DeskPhone
    , SoftPhone
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data PhoneType = PhoneType' (CI Text)
                   deriving (Eq, Ord, Read, Show, Data, Typeable,
                             Generic)

pattern DeskPhone :: PhoneType
pattern DeskPhone = PhoneType' "DESK_PHONE"

pattern SoftPhone :: PhoneType
pattern SoftPhone = PhoneType' "SOFT_PHONE"

{-# COMPLETE
  DeskPhone,
  SoftPhone,
  PhoneType' #-}

instance FromText PhoneType where
    parser = (PhoneType' . mk) <$> takeText

instance ToText PhoneType where
    toText (PhoneType' ci) = original ci

-- | Represents an enum of /known/ $PhoneType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum PhoneType where
    toEnum i = case i of
        0 -> DeskPhone
        1 -> SoftPhone
        _ -> (error . showText) $ "Unknown index for PhoneType: " <> toText i
    fromEnum x = case x of
        DeskPhone -> 0
        SoftPhone -> 1
        PhoneType' name -> (error . showText) $ "Unknown PhoneType: " <> original name

-- | Represents the bounds of /known/ $PhoneType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded PhoneType where
    minBound = DeskPhone
    maxBound = SoftPhone

instance Hashable     PhoneType
instance NFData       PhoneType
instance ToByteString PhoneType
instance ToQuery      PhoneType
instance ToHeader     PhoneType

instance ToJSON PhoneType where
    toJSON = toJSONText

instance FromJSON PhoneType where
    parseJSON = parseJSONText "PhoneType"
