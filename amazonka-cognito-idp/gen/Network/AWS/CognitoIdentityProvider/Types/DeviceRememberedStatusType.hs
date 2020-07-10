{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.DeviceRememberedStatusType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentityProvider.Types.DeviceRememberedStatusType (
  DeviceRememberedStatusType (
    ..
    , NotRemembered
    , Remembered
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data DeviceRememberedStatusType = DeviceRememberedStatusType' (CI
                                                                 Text)
                                    deriving (Eq, Ord, Read, Show, Data,
                                              Typeable, Generic)

pattern NotRemembered :: DeviceRememberedStatusType
pattern NotRemembered = DeviceRememberedStatusType' "not_remembered"

pattern Remembered :: DeviceRememberedStatusType
pattern Remembered = DeviceRememberedStatusType' "remembered"

{-# COMPLETE
  NotRemembered,
  Remembered,
  DeviceRememberedStatusType' #-}

instance FromText DeviceRememberedStatusType where
    parser = (DeviceRememberedStatusType' . mk) <$> takeText

instance ToText DeviceRememberedStatusType where
    toText (DeviceRememberedStatusType' ci) = original ci

-- | Represents an enum of /known/ $DeviceRememberedStatusType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DeviceRememberedStatusType where
    toEnum i = case i of
        0 -> NotRemembered
        1 -> Remembered
        _ -> (error . showText) $ "Unknown index for DeviceRememberedStatusType: " <> toText i
    fromEnum x = case x of
        NotRemembered -> 0
        Remembered -> 1
        DeviceRememberedStatusType' name -> (error . showText) $ "Unknown DeviceRememberedStatusType: " <> original name

-- | Represents the bounds of /known/ $DeviceRememberedStatusType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DeviceRememberedStatusType where
    minBound = NotRemembered
    maxBound = Remembered

instance Hashable     DeviceRememberedStatusType
instance NFData       DeviceRememberedStatusType
instance ToByteString DeviceRememberedStatusType
instance ToQuery      DeviceRememberedStatusType
instance ToHeader     DeviceRememberedStatusType

instance ToJSON DeviceRememberedStatusType where
    toJSON = toJSONText
