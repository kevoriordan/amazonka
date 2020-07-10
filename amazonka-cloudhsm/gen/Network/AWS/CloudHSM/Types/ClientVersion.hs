{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSM.Types.ClientVersion
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudHSM.Types.ClientVersion (
  ClientVersion (
    ..
    , VD5_1
    , VD5_3
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ClientVersion = ClientVersion' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern VD5_1 :: ClientVersion
pattern VD5_1 = ClientVersion' "5.1"

pattern VD5_3 :: ClientVersion
pattern VD5_3 = ClientVersion' "5.3"

{-# COMPLETE
  VD5_1,
  VD5_3,
  ClientVersion' #-}

instance FromText ClientVersion where
    parser = (ClientVersion' . mk) <$> takeText

instance ToText ClientVersion where
    toText (ClientVersion' ci) = original ci

-- | Represents an enum of /known/ $ClientVersion.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ClientVersion where
    toEnum i = case i of
        0 -> VD5_1
        1 -> VD5_3
        _ -> (error . showText) $ "Unknown index for ClientVersion: " <> toText i
    fromEnum x = case x of
        VD5_1 -> 0
        VD5_3 -> 1
        ClientVersion' name -> (error . showText) $ "Unknown ClientVersion: " <> original name

-- | Represents the bounds of /known/ $ClientVersion.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ClientVersion where
    minBound = VD5_1
    maxBound = VD5_3

instance Hashable     ClientVersion
instance NFData       ClientVersion
instance ToByteString ClientVersion
instance ToQuery      ClientVersion
instance ToHeader     ClientVersion

instance ToJSON ClientVersion where
    toJSON = toJSONText
