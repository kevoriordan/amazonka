{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.InstanceAccessProtocol
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.InstanceAccessProtocol (
  InstanceAccessProtocol (
    ..
    , Rdp
    , SSH
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data InstanceAccessProtocol = InstanceAccessProtocol' (CI
                                                         Text)
                                deriving (Eq, Ord, Read, Show, Data, Typeable,
                                          Generic)

pattern Rdp :: InstanceAccessProtocol
pattern Rdp = InstanceAccessProtocol' "rdp"

pattern SSH :: InstanceAccessProtocol
pattern SSH = InstanceAccessProtocol' "ssh"

{-# COMPLETE
  Rdp,
  SSH,
  InstanceAccessProtocol' #-}

instance FromText InstanceAccessProtocol where
    parser = (InstanceAccessProtocol' . mk) <$> takeText

instance ToText InstanceAccessProtocol where
    toText (InstanceAccessProtocol' ci) = original ci

-- | Represents an enum of /known/ $InstanceAccessProtocol.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum InstanceAccessProtocol where
    toEnum i = case i of
        0 -> Rdp
        1 -> SSH
        _ -> (error . showText) $ "Unknown index for InstanceAccessProtocol: " <> toText i
    fromEnum x = case x of
        Rdp -> 0
        SSH -> 1
        InstanceAccessProtocol' name -> (error . showText) $ "Unknown InstanceAccessProtocol: " <> original name

-- | Represents the bounds of /known/ $InstanceAccessProtocol.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded InstanceAccessProtocol where
    minBound = Rdp
    maxBound = SSH

instance Hashable     InstanceAccessProtocol
instance NFData       InstanceAccessProtocol
instance ToByteString InstanceAccessProtocol
instance ToQuery      InstanceAccessProtocol
instance ToHeader     InstanceAccessProtocol

instance ToJSON InstanceAccessProtocol where
    toJSON = toJSONText

instance FromJSON InstanceAccessProtocol where
    parseJSON = parseJSONText "InstanceAccessProtocol"
