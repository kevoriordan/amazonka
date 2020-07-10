{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.GlobalEndpointTokenVersion
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IAM.Types.GlobalEndpointTokenVersion (
  GlobalEndpointTokenVersion (
    ..
    , V1Token
    , V2Token
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data GlobalEndpointTokenVersion = GlobalEndpointTokenVersion' (CI
                                                                 Text)
                                    deriving (Eq, Ord, Read, Show, Data,
                                              Typeable, Generic)

pattern V1Token :: GlobalEndpointTokenVersion
pattern V1Token = GlobalEndpointTokenVersion' "v1Token"

pattern V2Token :: GlobalEndpointTokenVersion
pattern V2Token = GlobalEndpointTokenVersion' "v2Token"

{-# COMPLETE
  V1Token,
  V2Token,
  GlobalEndpointTokenVersion' #-}

instance FromText GlobalEndpointTokenVersion where
    parser = (GlobalEndpointTokenVersion' . mk) <$> takeText

instance ToText GlobalEndpointTokenVersion where
    toText (GlobalEndpointTokenVersion' ci) = original ci

-- | Represents an enum of /known/ $GlobalEndpointTokenVersion.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum GlobalEndpointTokenVersion where
    toEnum i = case i of
        0 -> V1Token
        1 -> V2Token
        _ -> (error . showText) $ "Unknown index for GlobalEndpointTokenVersion: " <> toText i
    fromEnum x = case x of
        V1Token -> 0
        V2Token -> 1
        GlobalEndpointTokenVersion' name -> (error . showText) $ "Unknown GlobalEndpointTokenVersion: " <> original name

-- | Represents the bounds of /known/ $GlobalEndpointTokenVersion.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded GlobalEndpointTokenVersion where
    minBound = V1Token
    maxBound = V2Token

instance Hashable     GlobalEndpointTokenVersion
instance NFData       GlobalEndpointTokenVersion
instance ToByteString GlobalEndpointTokenVersion
instance ToQuery      GlobalEndpointTokenVersion
instance ToHeader     GlobalEndpointTokenVersion
