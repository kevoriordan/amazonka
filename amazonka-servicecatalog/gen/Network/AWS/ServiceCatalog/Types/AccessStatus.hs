{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.AccessStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ServiceCatalog.Types.AccessStatus (
  AccessStatus (
    ..
    , Disabled
    , Enabled
    , UnderChange
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AccessStatus = AccessStatus' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern Disabled :: AccessStatus
pattern Disabled = AccessStatus' "DISABLED"

pattern Enabled :: AccessStatus
pattern Enabled = AccessStatus' "ENABLED"

pattern UnderChange :: AccessStatus
pattern UnderChange = AccessStatus' "UNDER_CHANGE"

{-# COMPLETE
  Disabled,
  Enabled,
  UnderChange,
  AccessStatus' #-}

instance FromText AccessStatus where
    parser = (AccessStatus' . mk) <$> takeText

instance ToText AccessStatus where
    toText (AccessStatus' ci) = original ci

-- | Represents an enum of /known/ $AccessStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AccessStatus where
    toEnum i = case i of
        0 -> Disabled
        1 -> Enabled
        2 -> UnderChange
        _ -> (error . showText) $ "Unknown index for AccessStatus: " <> toText i
    fromEnum x = case x of
        Disabled -> 0
        Enabled -> 1
        UnderChange -> 2
        AccessStatus' name -> (error . showText) $ "Unknown AccessStatus: " <> original name

-- | Represents the bounds of /known/ $AccessStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AccessStatus where
    minBound = Disabled
    maxBound = UnderChange

instance Hashable     AccessStatus
instance NFData       AccessStatus
instance ToByteString AccessStatus
instance ToQuery      AccessStatus
instance ToHeader     AccessStatus

instance FromJSON AccessStatus where
    parseJSON = parseJSONText "AccessStatus"
