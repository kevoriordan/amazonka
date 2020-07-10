{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.ProxyConfigurationType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECS.Types.ProxyConfigurationType (
  ProxyConfigurationType (
    ..
    , Appmesh
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ProxyConfigurationType = ProxyConfigurationType' (CI
                                                         Text)
                                deriving (Eq, Ord, Read, Show, Data, Typeable,
                                          Generic)

pattern Appmesh :: ProxyConfigurationType
pattern Appmesh = ProxyConfigurationType' "APPMESH"

{-# COMPLETE
  Appmesh,
  ProxyConfigurationType' #-}

instance FromText ProxyConfigurationType where
    parser = (ProxyConfigurationType' . mk) <$> takeText

instance ToText ProxyConfigurationType where
    toText (ProxyConfigurationType' ci) = original ci

-- | Represents an enum of /known/ $ProxyConfigurationType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ProxyConfigurationType where
    toEnum i = case i of
        0 -> Appmesh
        _ -> (error . showText) $ "Unknown index for ProxyConfigurationType: " <> toText i
    fromEnum x = case x of
        Appmesh -> 0
        ProxyConfigurationType' name -> (error . showText) $ "Unknown ProxyConfigurationType: " <> original name

-- | Represents the bounds of /known/ $ProxyConfigurationType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ProxyConfigurationType where
    minBound = Appmesh
    maxBound = Appmesh

instance Hashable     ProxyConfigurationType
instance NFData       ProxyConfigurationType
instance ToByteString ProxyConfigurationType
instance ToQuery      ProxyConfigurationType
instance ToHeader     ProxyConfigurationType

instance ToJSON ProxyConfigurationType where
    toJSON = toJSONText

instance FromJSON ProxyConfigurationType where
    parseJSON = parseJSONText "ProxyConfigurationType"
