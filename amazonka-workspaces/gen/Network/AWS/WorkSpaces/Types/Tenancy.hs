{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.Tenancy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkSpaces.Types.Tenancy (
  Tenancy (
    ..
    , Dedicated
    , Shared
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data Tenancy = Tenancy' (CI Text)
                 deriving (Eq, Ord, Read, Show, Data, Typeable,
                           Generic)

pattern Dedicated :: Tenancy
pattern Dedicated = Tenancy' "DEDICATED"

pattern Shared :: Tenancy
pattern Shared = Tenancy' "SHARED"

{-# COMPLETE
  Dedicated,
  Shared,
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
        1 -> Shared
        _ -> (error . showText) $ "Unknown index for Tenancy: " <> toText i
    fromEnum x = case x of
        Dedicated -> 0
        Shared -> 1
        Tenancy' name -> (error . showText) $ "Unknown Tenancy: " <> original name

-- | Represents the bounds of /known/ $Tenancy.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Tenancy where
    minBound = Dedicated
    maxBound = Shared

instance Hashable     Tenancy
instance NFData       Tenancy
instance ToByteString Tenancy
instance ToQuery      Tenancy
instance ToHeader     Tenancy

instance ToJSON Tenancy where
    toJSON = toJSONText

instance FromJSON Tenancy where
    parseJSON = parseJSONText "Tenancy"
