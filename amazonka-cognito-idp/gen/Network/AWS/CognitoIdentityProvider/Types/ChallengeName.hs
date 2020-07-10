{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.ChallengeName
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentityProvider.Types.ChallengeName (
  ChallengeName (
    ..
    , MFA
    , Password
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ChallengeName = ChallengeName' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern MFA :: ChallengeName
pattern MFA = ChallengeName' "Mfa"

pattern Password :: ChallengeName
pattern Password = ChallengeName' "Password"

{-# COMPLETE
  MFA,
  Password,
  ChallengeName' #-}

instance FromText ChallengeName where
    parser = (ChallengeName' . mk) <$> takeText

instance ToText ChallengeName where
    toText (ChallengeName' ci) = original ci

-- | Represents an enum of /known/ $ChallengeName.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ChallengeName where
    toEnum i = case i of
        0 -> MFA
        1 -> Password
        _ -> (error . showText) $ "Unknown index for ChallengeName: " <> toText i
    fromEnum x = case x of
        MFA -> 0
        Password -> 1
        ChallengeName' name -> (error . showText) $ "Unknown ChallengeName: " <> original name

-- | Represents the bounds of /known/ $ChallengeName.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ChallengeName where
    minBound = MFA
    maxBound = Password

instance Hashable     ChallengeName
instance NFData       ChallengeName
instance ToByteString ChallengeName
instance ToQuery      ChallengeName
instance ToHeader     ChallengeName

instance FromJSON ChallengeName where
    parseJSON = parseJSONText "ChallengeName"
