{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.LoadBalancerProtocol
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.LoadBalancerProtocol (
  LoadBalancerProtocol (
    ..
    , HTTP
    , HTTPHTTPS
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data LoadBalancerProtocol = LoadBalancerProtocol' (CI
                                                     Text)
                              deriving (Eq, Ord, Read, Show, Data, Typeable,
                                        Generic)

pattern HTTP :: LoadBalancerProtocol
pattern HTTP = LoadBalancerProtocol' "HTTP"

pattern HTTPHTTPS :: LoadBalancerProtocol
pattern HTTPHTTPS = LoadBalancerProtocol' "HTTP_HTTPS"

{-# COMPLETE
  HTTP,
  HTTPHTTPS,
  LoadBalancerProtocol' #-}

instance FromText LoadBalancerProtocol where
    parser = (LoadBalancerProtocol' . mk) <$> takeText

instance ToText LoadBalancerProtocol where
    toText (LoadBalancerProtocol' ci) = original ci

-- | Represents an enum of /known/ $LoadBalancerProtocol.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum LoadBalancerProtocol where
    toEnum i = case i of
        0 -> HTTP
        1 -> HTTPHTTPS
        _ -> (error . showText) $ "Unknown index for LoadBalancerProtocol: " <> toText i
    fromEnum x = case x of
        HTTP -> 0
        HTTPHTTPS -> 1
        LoadBalancerProtocol' name -> (error . showText) $ "Unknown LoadBalancerProtocol: " <> original name

-- | Represents the bounds of /known/ $LoadBalancerProtocol.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded LoadBalancerProtocol where
    minBound = HTTP
    maxBound = HTTPHTTPS

instance Hashable     LoadBalancerProtocol
instance NFData       LoadBalancerProtocol
instance ToByteString LoadBalancerProtocol
instance ToQuery      LoadBalancerProtocol
instance ToHeader     LoadBalancerProtocol

instance FromJSON LoadBalancerProtocol where
    parseJSON = parseJSONText "LoadBalancerProtocol"
