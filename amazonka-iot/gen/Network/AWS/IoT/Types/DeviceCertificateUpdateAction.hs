{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.DeviceCertificateUpdateAction
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.DeviceCertificateUpdateAction (
  DeviceCertificateUpdateAction (
    ..
    , DCUADeactivate
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data DeviceCertificateUpdateAction = DeviceCertificateUpdateAction' (CI
                                                                       Text)
                                       deriving (Eq, Ord, Read, Show, Data,
                                                 Typeable, Generic)

pattern DCUADeactivate :: DeviceCertificateUpdateAction
pattern DCUADeactivate = DeviceCertificateUpdateAction' "DEACTIVATE"

{-# COMPLETE
  DCUADeactivate,
  DeviceCertificateUpdateAction' #-}

instance FromText DeviceCertificateUpdateAction where
    parser = (DeviceCertificateUpdateAction' . mk) <$> takeText

instance ToText DeviceCertificateUpdateAction where
    toText (DeviceCertificateUpdateAction' ci) = original ci

-- | Represents an enum of /known/ $DeviceCertificateUpdateAction.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DeviceCertificateUpdateAction where
    toEnum i = case i of
        0 -> DCUADeactivate
        _ -> (error . showText) $ "Unknown index for DeviceCertificateUpdateAction: " <> toText i
    fromEnum x = case x of
        DCUADeactivate -> 0
        DeviceCertificateUpdateAction' name -> (error . showText) $ "Unknown DeviceCertificateUpdateAction: " <> original name

-- | Represents the bounds of /known/ $DeviceCertificateUpdateAction.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DeviceCertificateUpdateAction where
    minBound = DCUADeactivate
    maxBound = DCUADeactivate

instance Hashable     DeviceCertificateUpdateAction
instance NFData       DeviceCertificateUpdateAction
instance ToByteString DeviceCertificateUpdateAction
instance ToQuery      DeviceCertificateUpdateAction
instance ToHeader     DeviceCertificateUpdateAction

instance ToJSON DeviceCertificateUpdateAction where
    toJSON = toJSONText

instance FromJSON DeviceCertificateUpdateAction where
    parseJSON = parseJSONText "DeviceCertificateUpdateAction"
