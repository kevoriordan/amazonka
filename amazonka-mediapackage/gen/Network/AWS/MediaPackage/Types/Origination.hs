{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.Origination
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaPackage.Types.Origination (
  Origination (
    ..
    , Allow
    , Deny
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data Origination = Origination' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern Allow :: Origination
pattern Allow = Origination' "ALLOW"

pattern Deny :: Origination
pattern Deny = Origination' "DENY"

{-# COMPLETE
  Allow,
  Deny,
  Origination' #-}

instance FromText Origination where
    parser = (Origination' . mk) <$> takeText

instance ToText Origination where
    toText (Origination' ci) = original ci

-- | Represents an enum of /known/ $Origination.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Origination where
    toEnum i = case i of
        0 -> Allow
        1 -> Deny
        _ -> (error . showText) $ "Unknown index for Origination: " <> toText i
    fromEnum x = case x of
        Allow -> 0
        Deny -> 1
        Origination' name -> (error . showText) $ "Unknown Origination: " <> original name

-- | Represents the bounds of /known/ $Origination.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Origination where
    minBound = Allow
    maxBound = Deny

instance Hashable     Origination
instance NFData       Origination
instance ToByteString Origination
instance ToQuery      Origination
instance ToHeader     Origination

instance ToJSON Origination where
    toJSON = toJSONText

instance FromJSON Origination where
    parseJSON = parseJSONText "Origination"
