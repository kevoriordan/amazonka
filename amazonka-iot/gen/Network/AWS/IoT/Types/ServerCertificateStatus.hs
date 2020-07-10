{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ServerCertificateStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.ServerCertificateStatus (
  ServerCertificateStatus (
    ..
    , Invalid
    , Valid
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ServerCertificateStatus = ServerCertificateStatus' (CI
                                                           Text)
                                 deriving (Eq, Ord, Read, Show, Data, Typeable,
                                           Generic)

pattern Invalid :: ServerCertificateStatus
pattern Invalid = ServerCertificateStatus' "INVALID"

pattern Valid :: ServerCertificateStatus
pattern Valid = ServerCertificateStatus' "VALID"

{-# COMPLETE
  Invalid,
  Valid,
  ServerCertificateStatus' #-}

instance FromText ServerCertificateStatus where
    parser = (ServerCertificateStatus' . mk) <$> takeText

instance ToText ServerCertificateStatus where
    toText (ServerCertificateStatus' ci) = original ci

-- | Represents an enum of /known/ $ServerCertificateStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ServerCertificateStatus where
    toEnum i = case i of
        0 -> Invalid
        1 -> Valid
        _ -> (error . showText) $ "Unknown index for ServerCertificateStatus: " <> toText i
    fromEnum x = case x of
        Invalid -> 0
        Valid -> 1
        ServerCertificateStatus' name -> (error . showText) $ "Unknown ServerCertificateStatus: " <> original name

-- | Represents the bounds of /known/ $ServerCertificateStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ServerCertificateStatus where
    minBound = Invalid
    maxBound = Valid

instance Hashable     ServerCertificateStatus
instance NFData       ServerCertificateStatus
instance ToByteString ServerCertificateStatus
instance ToQuery      ServerCertificateStatus
instance ToHeader     ServerCertificateStatus

instance FromJSON ServerCertificateStatus where
    parseJSON = parseJSONText "ServerCertificateStatus"
