{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.ActionTypeEnum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ELBv2.Types.ActionTypeEnum (
  ActionTypeEnum (
    ..
    , AuthenticateCognito
    , AuthenticateOidc
    , FixedResponse
    , Forward
    , Redirect
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ActionTypeEnum = ActionTypeEnum' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern AuthenticateCognito :: ActionTypeEnum
pattern AuthenticateCognito = ActionTypeEnum' "authenticate-cognito"

pattern AuthenticateOidc :: ActionTypeEnum
pattern AuthenticateOidc = ActionTypeEnum' "authenticate-oidc"

pattern FixedResponse :: ActionTypeEnum
pattern FixedResponse = ActionTypeEnum' "fixed-response"

pattern Forward :: ActionTypeEnum
pattern Forward = ActionTypeEnum' "forward"

pattern Redirect :: ActionTypeEnum
pattern Redirect = ActionTypeEnum' "redirect"

{-# COMPLETE
  AuthenticateCognito,
  AuthenticateOidc,
  FixedResponse,
  Forward,
  Redirect,
  ActionTypeEnum' #-}

instance FromText ActionTypeEnum where
    parser = (ActionTypeEnum' . mk) <$> takeText

instance ToText ActionTypeEnum where
    toText (ActionTypeEnum' ci) = original ci

-- | Represents an enum of /known/ $ActionTypeEnum.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ActionTypeEnum where
    toEnum i = case i of
        0 -> AuthenticateCognito
        1 -> AuthenticateOidc
        2 -> FixedResponse
        3 -> Forward
        4 -> Redirect
        _ -> (error . showText) $ "Unknown index for ActionTypeEnum: " <> toText i
    fromEnum x = case x of
        AuthenticateCognito -> 0
        AuthenticateOidc -> 1
        FixedResponse -> 2
        Forward -> 3
        Redirect -> 4
        ActionTypeEnum' name -> (error . showText) $ "Unknown ActionTypeEnum: " <> original name

-- | Represents the bounds of /known/ $ActionTypeEnum.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ActionTypeEnum where
    minBound = AuthenticateCognito
    maxBound = Redirect

instance Hashable     ActionTypeEnum
instance NFData       ActionTypeEnum
instance ToByteString ActionTypeEnum
instance ToQuery      ActionTypeEnum
instance ToHeader     ActionTypeEnum

instance FromXML ActionTypeEnum where
    parseXML = parseXMLText "ActionTypeEnum"
