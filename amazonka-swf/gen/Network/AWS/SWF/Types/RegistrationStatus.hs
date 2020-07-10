{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.RegistrationStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SWF.Types.RegistrationStatus (
  RegistrationStatus (
    ..
    , Deprecated
    , Registered
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data RegistrationStatus = RegistrationStatus' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern Deprecated :: RegistrationStatus
pattern Deprecated = RegistrationStatus' "DEPRECATED"

pattern Registered :: RegistrationStatus
pattern Registered = RegistrationStatus' "REGISTERED"

{-# COMPLETE
  Deprecated,
  Registered,
  RegistrationStatus' #-}

instance FromText RegistrationStatus where
    parser = (RegistrationStatus' . mk) <$> takeText

instance ToText RegistrationStatus where
    toText (RegistrationStatus' ci) = original ci

-- | Represents an enum of /known/ $RegistrationStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum RegistrationStatus where
    toEnum i = case i of
        0 -> Deprecated
        1 -> Registered
        _ -> (error . showText) $ "Unknown index for RegistrationStatus: " <> toText i
    fromEnum x = case x of
        Deprecated -> 0
        Registered -> 1
        RegistrationStatus' name -> (error . showText) $ "Unknown RegistrationStatus: " <> original name

-- | Represents the bounds of /known/ $RegistrationStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded RegistrationStatus where
    minBound = Deprecated
    maxBound = Registered

instance Hashable     RegistrationStatus
instance NFData       RegistrationStatus
instance ToByteString RegistrationStatus
instance ToQuery      RegistrationStatus
instance ToHeader     RegistrationStatus

instance ToJSON RegistrationStatus where
    toJSON = toJSONText

instance FromJSON RegistrationStatus where
    parseJSON = parseJSONText "RegistrationStatus"
