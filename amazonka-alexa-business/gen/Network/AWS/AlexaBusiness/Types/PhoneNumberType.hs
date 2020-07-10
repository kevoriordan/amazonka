{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.PhoneNumberType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AlexaBusiness.Types.PhoneNumberType (
  PhoneNumberType (
    ..
    , PNTHome
    , PNTMobile
    , PNTWork
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data PhoneNumberType = PhoneNumberType' (CI Text)
                         deriving (Eq, Ord, Show, Data, Typeable, Generic)

pattern PNTHome :: PhoneNumberType
pattern PNTHome = PhoneNumberType' "HOME"

pattern PNTMobile :: PhoneNumberType
pattern PNTMobile = PhoneNumberType' "MOBILE"

pattern PNTWork :: PhoneNumberType
pattern PNTWork = PhoneNumberType' "WORK"

{-# COMPLETE
  PNTHome,
  PNTMobile,
  PNTWork,
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
        0 -> PNTHome
        1 -> PNTMobile
        2 -> PNTWork
        _ -> (error . showText) $ "Unknown index for PhoneNumberType: " <> toText i
    fromEnum x = case x of
        PNTHome -> 0
        PNTMobile -> 1
        PNTWork -> 2
        PhoneNumberType' name -> (error . showText) $ "Unknown PhoneNumberType: " <> original name

-- | Represents the bounds of /known/ $PhoneNumberType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded PhoneNumberType where
    minBound = PNTHome
    maxBound = PNTWork

instance Hashable     PhoneNumberType
instance NFData       PhoneNumberType
instance ToByteString PhoneNumberType
instance ToQuery      PhoneNumberType
instance ToHeader     PhoneNumberType

instance ToJSON PhoneNumberType where
    toJSON = toJSONText

instance FromJSON PhoneNumberType where
    parseJSON = parseJSONText "PhoneNumberType"
