{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DNSNameState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.DNSNameState (
  DNSNameState (
    ..
    , DNSFailed
    , DNSPendingVerification
    , DNSVerified
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data DNSNameState = DNSNameState' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern DNSFailed :: DNSNameState
pattern DNSFailed = DNSNameState' "failed"

pattern DNSPendingVerification :: DNSNameState
pattern DNSPendingVerification = DNSNameState' "pendingVerification"

pattern DNSVerified :: DNSNameState
pattern DNSVerified = DNSNameState' "verified"

{-# COMPLETE
  DNSFailed,
  DNSPendingVerification,
  DNSVerified,
  DNSNameState' #-}

instance FromText DNSNameState where
    parser = (DNSNameState' . mk) <$> takeText

instance ToText DNSNameState where
    toText (DNSNameState' ci) = original ci

-- | Represents an enum of /known/ $DNSNameState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DNSNameState where
    toEnum i = case i of
        0 -> DNSFailed
        1 -> DNSPendingVerification
        2 -> DNSVerified
        _ -> (error . showText) $ "Unknown index for DNSNameState: " <> toText i
    fromEnum x = case x of
        DNSFailed -> 0
        DNSPendingVerification -> 1
        DNSVerified -> 2
        DNSNameState' name -> (error . showText) $ "Unknown DNSNameState: " <> original name

-- | Represents the bounds of /known/ $DNSNameState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DNSNameState where
    minBound = DNSFailed
    maxBound = DNSVerified

instance Hashable     DNSNameState
instance NFData       DNSNameState
instance ToByteString DNSNameState
instance ToQuery      DNSNameState
instance ToHeader     DNSNameState

instance FromXML DNSNameState where
    parseXML = parseXMLText "DNSNameState"
