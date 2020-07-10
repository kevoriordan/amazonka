{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.NetworkInputServerValidation
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.NetworkInputServerValidation (
  NetworkInputServerValidation (
    ..
    , CheckCryptographyAndValidateName
    , CheckCryptographyOnly
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Network Input Server Validation
data NetworkInputServerValidation = NetworkInputServerValidation' (CI
                                                                     Text)
                                      deriving (Eq, Ord, Read, Show, Data,
                                                Typeable, Generic)

pattern CheckCryptographyAndValidateName :: NetworkInputServerValidation
pattern CheckCryptographyAndValidateName = NetworkInputServerValidation' "CHECK_CRYPTOGRAPHY_AND_VALIDATE_NAME"

pattern CheckCryptographyOnly :: NetworkInputServerValidation
pattern CheckCryptographyOnly = NetworkInputServerValidation' "CHECK_CRYPTOGRAPHY_ONLY"

{-# COMPLETE
  CheckCryptographyAndValidateName,
  CheckCryptographyOnly,
  NetworkInputServerValidation' #-}

instance FromText NetworkInputServerValidation where
    parser = (NetworkInputServerValidation' . mk) <$> takeText

instance ToText NetworkInputServerValidation where
    toText (NetworkInputServerValidation' ci) = original ci

-- | Represents an enum of /known/ $NetworkInputServerValidation.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum NetworkInputServerValidation where
    toEnum i = case i of
        0 -> CheckCryptographyAndValidateName
        1 -> CheckCryptographyOnly
        _ -> (error . showText) $ "Unknown index for NetworkInputServerValidation: " <> toText i
    fromEnum x = case x of
        CheckCryptographyAndValidateName -> 0
        CheckCryptographyOnly -> 1
        NetworkInputServerValidation' name -> (error . showText) $ "Unknown NetworkInputServerValidation: " <> original name

-- | Represents the bounds of /known/ $NetworkInputServerValidation.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded NetworkInputServerValidation where
    minBound = CheckCryptographyAndValidateName
    maxBound = CheckCryptographyOnly

instance Hashable     NetworkInputServerValidation
instance NFData       NetworkInputServerValidation
instance ToByteString NetworkInputServerValidation
instance ToQuery      NetworkInputServerValidation
instance ToHeader     NetworkInputServerValidation

instance ToJSON NetworkInputServerValidation where
    toJSON = toJSONText

instance FromJSON NetworkInputServerValidation where
    parseJSON = parseJSONText "NetworkInputServerValidation"
