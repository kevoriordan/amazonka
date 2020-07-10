{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.Types.RenewalEligibility
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CertificateManager.Types.RenewalEligibility (
  RenewalEligibility (
    ..
    , Eligible
    , Ineligible
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data RenewalEligibility = RenewalEligibility' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern Eligible :: RenewalEligibility
pattern Eligible = RenewalEligibility' "ELIGIBLE"

pattern Ineligible :: RenewalEligibility
pattern Ineligible = RenewalEligibility' "INELIGIBLE"

{-# COMPLETE
  Eligible,
  Ineligible,
  RenewalEligibility' #-}

instance FromText RenewalEligibility where
    parser = (RenewalEligibility' . mk) <$> takeText

instance ToText RenewalEligibility where
    toText (RenewalEligibility' ci) = original ci

-- | Represents an enum of /known/ $RenewalEligibility.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum RenewalEligibility where
    toEnum i = case i of
        0 -> Eligible
        1 -> Ineligible
        _ -> (error . showText) $ "Unknown index for RenewalEligibility: " <> toText i
    fromEnum x = case x of
        Eligible -> 0
        Ineligible -> 1
        RenewalEligibility' name -> (error . showText) $ "Unknown RenewalEligibility: " <> original name

-- | Represents the bounds of /known/ $RenewalEligibility.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded RenewalEligibility where
    minBound = Eligible
    maxBound = Ineligible

instance Hashable     RenewalEligibility
instance NFData       RenewalEligibility
instance ToByteString RenewalEligibility
instance ToQuery      RenewalEligibility
instance ToHeader     RenewalEligibility

instance FromJSON RenewalEligibility where
    parseJSON = parseJSONText "RenewalEligibility"
