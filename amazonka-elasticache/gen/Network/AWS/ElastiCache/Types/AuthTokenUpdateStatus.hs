{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.AuthTokenUpdateStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElastiCache.Types.AuthTokenUpdateStatus (
  AuthTokenUpdateStatus (
    ..
    , Rotating
    , Setting
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AuthTokenUpdateStatus = AuthTokenUpdateStatus' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern Rotating :: AuthTokenUpdateStatus
pattern Rotating = AuthTokenUpdateStatus' "ROTATING"

pattern Setting :: AuthTokenUpdateStatus
pattern Setting = AuthTokenUpdateStatus' "SETTING"

{-# COMPLETE
  Rotating,
  Setting,
  AuthTokenUpdateStatus' #-}

instance FromText AuthTokenUpdateStatus where
    parser = (AuthTokenUpdateStatus' . mk) <$> takeText

instance ToText AuthTokenUpdateStatus where
    toText (AuthTokenUpdateStatus' ci) = original ci

-- | Represents an enum of /known/ $AuthTokenUpdateStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AuthTokenUpdateStatus where
    toEnum i = case i of
        0 -> Rotating
        1 -> Setting
        _ -> (error . showText) $ "Unknown index for AuthTokenUpdateStatus: " <> toText i
    fromEnum x = case x of
        Rotating -> 0
        Setting -> 1
        AuthTokenUpdateStatus' name -> (error . showText) $ "Unknown AuthTokenUpdateStatus: " <> original name

-- | Represents the bounds of /known/ $AuthTokenUpdateStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AuthTokenUpdateStatus where
    minBound = Rotating
    maxBound = Setting

instance Hashable     AuthTokenUpdateStatus
instance NFData       AuthTokenUpdateStatus
instance ToByteString AuthTokenUpdateStatus
instance ToQuery      AuthTokenUpdateStatus
instance ToHeader     AuthTokenUpdateStatus

instance FromXML AuthTokenUpdateStatus where
    parseXML = parseXMLText "AuthTokenUpdateStatus"
