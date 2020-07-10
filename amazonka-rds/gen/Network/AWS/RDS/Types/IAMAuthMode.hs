{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.IAMAuthMode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.RDS.Types.IAMAuthMode (
  IAMAuthMode (
    ..
    , Disabled
    , Required
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data IAMAuthMode = IAMAuthMode' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern Disabled :: IAMAuthMode
pattern Disabled = IAMAuthMode' "DISABLED"

pattern Required :: IAMAuthMode
pattern Required = IAMAuthMode' "REQUIRED"

{-# COMPLETE
  Disabled,
  Required,
  IAMAuthMode' #-}

instance FromText IAMAuthMode where
    parser = (IAMAuthMode' . mk) <$> takeText

instance ToText IAMAuthMode where
    toText (IAMAuthMode' ci) = original ci

-- | Represents an enum of /known/ $IAMAuthMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum IAMAuthMode where
    toEnum i = case i of
        0 -> Disabled
        1 -> Required
        _ -> (error . showText) $ "Unknown index for IAMAuthMode: " <> toText i
    fromEnum x = case x of
        Disabled -> 0
        Required -> 1
        IAMAuthMode' name -> (error . showText) $ "Unknown IAMAuthMode: " <> original name

-- | Represents the bounds of /known/ $IAMAuthMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded IAMAuthMode where
    minBound = Disabled
    maxBound = Required

instance Hashable     IAMAuthMode
instance NFData       IAMAuthMode
instance ToByteString IAMAuthMode
instance ToQuery      IAMAuthMode
instance ToHeader     IAMAuthMode

instance FromXML IAMAuthMode where
    parseXML = parseXMLText "IAMAuthMode"
