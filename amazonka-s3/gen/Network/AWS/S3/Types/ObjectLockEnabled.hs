{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ObjectLockEnabled
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.ObjectLockEnabled (
  ObjectLockEnabled (
    ..
    , OLEEnabled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude
import Network.AWS.S3.Internal

data ObjectLockEnabled = ObjectLockEnabled' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern OLEEnabled :: ObjectLockEnabled
pattern OLEEnabled = ObjectLockEnabled' "Enabled"

{-# COMPLETE
  OLEEnabled,
  ObjectLockEnabled' #-}

instance FromText ObjectLockEnabled where
    parser = (ObjectLockEnabled' . mk) <$> takeText

instance ToText ObjectLockEnabled where
    toText (ObjectLockEnabled' ci) = original ci

-- | Represents an enum of /known/ $ObjectLockEnabled.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ObjectLockEnabled where
    toEnum i = case i of
        0 -> OLEEnabled
        _ -> (error . showText) $ "Unknown index for ObjectLockEnabled: " <> toText i
    fromEnum x = case x of
        OLEEnabled -> 0
        ObjectLockEnabled' name -> (error . showText) $ "Unknown ObjectLockEnabled: " <> original name

-- | Represents the bounds of /known/ $ObjectLockEnabled.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ObjectLockEnabled where
    minBound = OLEEnabled
    maxBound = OLEEnabled

instance Hashable     ObjectLockEnabled
instance NFData       ObjectLockEnabled
instance ToByteString ObjectLockEnabled
instance ToQuery      ObjectLockEnabled
instance ToHeader     ObjectLockEnabled

instance FromXML ObjectLockEnabled where
    parseXML = parseXMLText "ObjectLockEnabled"

instance ToXML ObjectLockEnabled where
    toXML = toXMLText
