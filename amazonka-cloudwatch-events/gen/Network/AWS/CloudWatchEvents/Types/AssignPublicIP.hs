{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.AssignPublicIP
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudWatchEvents.Types.AssignPublicIP (
  AssignPublicIP (
    ..
    , APIDisabled
    , APIEnabled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AssignPublicIP = AssignPublicIP' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern APIDisabled :: AssignPublicIP
pattern APIDisabled = AssignPublicIP' "DISABLED"

pattern APIEnabled :: AssignPublicIP
pattern APIEnabled = AssignPublicIP' "ENABLED"

{-# COMPLETE
  APIDisabled,
  APIEnabled,
  AssignPublicIP' #-}

instance FromText AssignPublicIP where
    parser = (AssignPublicIP' . mk) <$> takeText

instance ToText AssignPublicIP where
    toText (AssignPublicIP' ci) = original ci

-- | Represents an enum of /known/ $AssignPublicIP.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AssignPublicIP where
    toEnum i = case i of
        0 -> APIDisabled
        1 -> APIEnabled
        _ -> (error . showText) $ "Unknown index for AssignPublicIP: " <> toText i
    fromEnum x = case x of
        APIDisabled -> 0
        APIEnabled -> 1
        AssignPublicIP' name -> (error . showText) $ "Unknown AssignPublicIP: " <> original name

-- | Represents the bounds of /known/ $AssignPublicIP.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AssignPublicIP where
    minBound = APIDisabled
    maxBound = APIEnabled

instance Hashable     AssignPublicIP
instance NFData       AssignPublicIP
instance ToByteString AssignPublicIP
instance ToQuery      AssignPublicIP
instance ToHeader     AssignPublicIP

instance ToJSON AssignPublicIP where
    toJSON = toJSONText

instance FromJSON AssignPublicIP where
    parseJSON = parseJSONText "AssignPublicIP"
