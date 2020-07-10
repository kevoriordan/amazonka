{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Tenancy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.Tenancy (
  Tenancy (
    ..
    , Dedicated
    , Default
    , Host
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data Tenancy = Tenancy' (CI Text)
                 deriving (Eq, Ord, Read, Show, Data, Typeable,
                           Generic)

pattern Dedicated :: Tenancy
pattern Dedicated = Tenancy' "dedicated"

pattern Default :: Tenancy
pattern Default = Tenancy' "default"

pattern Host :: Tenancy
pattern Host = Tenancy' "host"

{-# COMPLETE
  Dedicated,
  Default,
  Host,
  Tenancy' #-}

instance FromText Tenancy where
    parser = (Tenancy' . mk) <$> takeText

instance ToText Tenancy where
    toText (Tenancy' ci) = original ci

-- | Represents an enum of /known/ $Tenancy.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Tenancy where
    toEnum i = case i of
        0 -> Dedicated
        1 -> Default
        2 -> Host
        _ -> (error . showText) $ "Unknown index for Tenancy: " <> toText i
    fromEnum x = case x of
        Dedicated -> 0
        Default -> 1
        Host -> 2
        Tenancy' name -> (error . showText) $ "Unknown Tenancy: " <> original name

-- | Represents the bounds of /known/ $Tenancy.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Tenancy where
    minBound = Dedicated
    maxBound = Host

instance Hashable     Tenancy
instance NFData       Tenancy
instance ToByteString Tenancy
instance ToQuery      Tenancy
instance ToHeader     Tenancy

instance FromXML Tenancy where
    parseXML = parseXMLText "Tenancy"
