{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.ChallengeResponse
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentityProvider.Types.ChallengeResponse (
  ChallengeResponse (
    ..
    , CFailure
    , CSuccess
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ChallengeResponse = ChallengeResponse' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern CFailure :: ChallengeResponse
pattern CFailure = ChallengeResponse' "Failure"

pattern CSuccess :: ChallengeResponse
pattern CSuccess = ChallengeResponse' "Success"

{-# COMPLETE
  CFailure,
  CSuccess,
  ChallengeResponse' #-}

instance FromText ChallengeResponse where
    parser = (ChallengeResponse' . mk) <$> takeText

instance ToText ChallengeResponse where
    toText (ChallengeResponse' ci) = original ci

-- | Represents an enum of /known/ $ChallengeResponse.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ChallengeResponse where
    toEnum i = case i of
        0 -> CFailure
        1 -> CSuccess
        _ -> (error . showText) $ "Unknown index for ChallengeResponse: " <> toText i
    fromEnum x = case x of
        CFailure -> 0
        CSuccess -> 1
        ChallengeResponse' name -> (error . showText) $ "Unknown ChallengeResponse: " <> original name

-- | Represents the bounds of /known/ $ChallengeResponse.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ChallengeResponse where
    minBound = CFailure
    maxBound = CSuccess

instance Hashable     ChallengeResponse
instance NFData       ChallengeResponse
instance ToByteString ChallengeResponse
instance ToQuery      ChallengeResponse
instance ToHeader     ChallengeResponse

instance FromJSON ChallengeResponse where
    parseJSON = parseJSONText "ChallengeResponse"
