{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ObjectLockMode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.ObjectLockMode (
  ObjectLockMode (
    ..
    , Compliance
    , Governance
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude
import Network.AWS.S3.Internal

data ObjectLockMode = ObjectLockMode' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern Compliance :: ObjectLockMode
pattern Compliance = ObjectLockMode' "COMPLIANCE"

pattern Governance :: ObjectLockMode
pattern Governance = ObjectLockMode' "GOVERNANCE"

{-# COMPLETE
  Compliance,
  Governance,
  ObjectLockMode' #-}

instance FromText ObjectLockMode where
    parser = (ObjectLockMode' . mk) <$> takeText

instance ToText ObjectLockMode where
    toText (ObjectLockMode' ci) = original ci

-- | Represents an enum of /known/ $ObjectLockMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ObjectLockMode where
    toEnum i = case i of
        0 -> Compliance
        1 -> Governance
        _ -> (error . showText) $ "Unknown index for ObjectLockMode: " <> toText i
    fromEnum x = case x of
        Compliance -> 0
        Governance -> 1
        ObjectLockMode' name -> (error . showText) $ "Unknown ObjectLockMode: " <> original name

-- | Represents the bounds of /known/ $ObjectLockMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ObjectLockMode where
    minBound = Compliance
    maxBound = Governance

instance Hashable     ObjectLockMode
instance NFData       ObjectLockMode
instance ToByteString ObjectLockMode
instance ToQuery      ObjectLockMode
instance ToHeader     ObjectLockMode

instance FromXML ObjectLockMode where
    parseXML = parseXMLText "ObjectLockMode"

instance ToXML ObjectLockMode where
    toXML = toXMLText
