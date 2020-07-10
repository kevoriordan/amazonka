{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.CertificateState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DirectoryService.Types.CertificateState (
  CertificateState (
    ..
    , DeregisterFailed
    , Deregistered
    , Deregistering
    , RegisterFailed
    , Registered
    , Registering
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data CertificateState = CertificateState' (CI Text)
                          deriving (Eq, Ord, Read, Show, Data, Typeable,
                                    Generic)

pattern DeregisterFailed :: CertificateState
pattern DeregisterFailed = CertificateState' "DeregisterFailed"

pattern Deregistered :: CertificateState
pattern Deregistered = CertificateState' "Deregistered"

pattern Deregistering :: CertificateState
pattern Deregistering = CertificateState' "Deregistering"

pattern RegisterFailed :: CertificateState
pattern RegisterFailed = CertificateState' "RegisterFailed"

pattern Registered :: CertificateState
pattern Registered = CertificateState' "Registered"

pattern Registering :: CertificateState
pattern Registering = CertificateState' "Registering"

{-# COMPLETE
  DeregisterFailed,
  Deregistered,
  Deregistering,
  RegisterFailed,
  Registered,
  Registering,
  CertificateState' #-}

instance FromText CertificateState where
    parser = (CertificateState' . mk) <$> takeText

instance ToText CertificateState where
    toText (CertificateState' ci) = original ci

-- | Represents an enum of /known/ $CertificateState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum CertificateState where
    toEnum i = case i of
        0 -> DeregisterFailed
        1 -> Deregistered
        2 -> Deregistering
        3 -> RegisterFailed
        4 -> Registered
        5 -> Registering
        _ -> (error . showText) $ "Unknown index for CertificateState: " <> toText i
    fromEnum x = case x of
        DeregisterFailed -> 0
        Deregistered -> 1
        Deregistering -> 2
        RegisterFailed -> 3
        Registered -> 4
        Registering -> 5
        CertificateState' name -> (error . showText) $ "Unknown CertificateState: " <> original name

-- | Represents the bounds of /known/ $CertificateState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded CertificateState where
    minBound = DeregisterFailed
    maxBound = Registering

instance Hashable     CertificateState
instance NFData       CertificateState
instance ToByteString CertificateState
instance ToQuery      CertificateState
instance ToHeader     CertificateState

instance FromJSON CertificateState where
    parseJSON = parseJSONText "CertificateState"
