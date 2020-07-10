{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.HostTenancy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.HostTenancy (
  HostTenancy (
    ..
    , HTDedicated
    , HTHost
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data HostTenancy = HostTenancy' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern HTDedicated :: HostTenancy
pattern HTDedicated = HostTenancy' "dedicated"

pattern HTHost :: HostTenancy
pattern HTHost = HostTenancy' "host"

{-# COMPLETE
  HTDedicated,
  HTHost,
  HostTenancy' #-}

instance FromText HostTenancy where
    parser = (HostTenancy' . mk) <$> takeText

instance ToText HostTenancy where
    toText (HostTenancy' ci) = original ci

-- | Represents an enum of /known/ $HostTenancy.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum HostTenancy where
    toEnum i = case i of
        0 -> HTDedicated
        1 -> HTHost
        _ -> (error . showText) $ "Unknown index for HostTenancy: " <> toText i
    fromEnum x = case x of
        HTDedicated -> 0
        HTHost -> 1
        HostTenancy' name -> (error . showText) $ "Unknown HostTenancy: " <> original name

-- | Represents the bounds of /known/ $HostTenancy.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded HostTenancy where
    minBound = HTDedicated
    maxBound = HTHost

instance Hashable     HostTenancy
instance NFData       HostTenancy
instance ToByteString HostTenancy
instance ToQuery      HostTenancy
instance ToHeader     HostTenancy
