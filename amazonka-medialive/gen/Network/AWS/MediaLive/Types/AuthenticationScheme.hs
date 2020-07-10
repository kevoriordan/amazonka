{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AuthenticationScheme
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.AuthenticationScheme (
  AuthenticationScheme (
    ..
    , Akamai
    , Common
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Authentication Scheme
data AuthenticationScheme = AuthenticationScheme' (CI
                                                     Text)
                              deriving (Eq, Ord, Read, Show, Data, Typeable,
                                        Generic)

pattern Akamai :: AuthenticationScheme
pattern Akamai = AuthenticationScheme' "AKAMAI"

pattern Common :: AuthenticationScheme
pattern Common = AuthenticationScheme' "COMMON"

{-# COMPLETE
  Akamai,
  Common,
  AuthenticationScheme' #-}

instance FromText AuthenticationScheme where
    parser = (AuthenticationScheme' . mk) <$> takeText

instance ToText AuthenticationScheme where
    toText (AuthenticationScheme' ci) = original ci

-- | Represents an enum of /known/ $AuthenticationScheme.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AuthenticationScheme where
    toEnum i = case i of
        0 -> Akamai
        1 -> Common
        _ -> (error . showText) $ "Unknown index for AuthenticationScheme: " <> toText i
    fromEnum x = case x of
        Akamai -> 0
        Common -> 1
        AuthenticationScheme' name -> (error . showText) $ "Unknown AuthenticationScheme: " <> original name

-- | Represents the bounds of /known/ $AuthenticationScheme.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AuthenticationScheme where
    minBound = Akamai
    maxBound = Common

instance Hashable     AuthenticationScheme
instance NFData       AuthenticationScheme
instance ToByteString AuthenticationScheme
instance ToQuery      AuthenticationScheme
instance ToHeader     AuthenticationScheme

instance ToJSON AuthenticationScheme where
    toJSON = toJSONText

instance FromJSON AuthenticationScheme where
    parseJSON = parseJSONText "AuthenticationScheme"
