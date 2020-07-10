{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AutoRegistrationStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.AutoRegistrationStatus (
  AutoRegistrationStatus (
    ..
    , Disable
    , Enable
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AutoRegistrationStatus = AutoRegistrationStatus' (CI
                                                         Text)
                                deriving (Eq, Ord, Read, Show, Data, Typeable,
                                          Generic)

pattern Disable :: AutoRegistrationStatus
pattern Disable = AutoRegistrationStatus' "DISABLE"

pattern Enable :: AutoRegistrationStatus
pattern Enable = AutoRegistrationStatus' "ENABLE"

{-# COMPLETE
  Disable,
  Enable,
  AutoRegistrationStatus' #-}

instance FromText AutoRegistrationStatus where
    parser = (AutoRegistrationStatus' . mk) <$> takeText

instance ToText AutoRegistrationStatus where
    toText (AutoRegistrationStatus' ci) = original ci

-- | Represents an enum of /known/ $AutoRegistrationStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AutoRegistrationStatus where
    toEnum i = case i of
        0 -> Disable
        1 -> Enable
        _ -> (error . showText) $ "Unknown index for AutoRegistrationStatus: " <> toText i
    fromEnum x = case x of
        Disable -> 0
        Enable -> 1
        AutoRegistrationStatus' name -> (error . showText) $ "Unknown AutoRegistrationStatus: " <> original name

-- | Represents the bounds of /known/ $AutoRegistrationStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AutoRegistrationStatus where
    minBound = Disable
    maxBound = Enable

instance Hashable     AutoRegistrationStatus
instance NFData       AutoRegistrationStatus
instance ToByteString AutoRegistrationStatus
instance ToQuery      AutoRegistrationStatus
instance ToHeader     AutoRegistrationStatus

instance ToJSON AutoRegistrationStatus where
    toJSON = toJSONText

instance FromJSON AutoRegistrationStatus where
    parseJSON = parseJSONText "AutoRegistrationStatus"
