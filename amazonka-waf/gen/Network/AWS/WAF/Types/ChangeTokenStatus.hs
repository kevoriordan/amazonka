{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.ChangeTokenStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WAF.Types.ChangeTokenStatus (
  ChangeTokenStatus (
    ..
    , Insync
    , Pending
    , Provisioned
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ChangeTokenStatus = ChangeTokenStatus' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern Insync :: ChangeTokenStatus
pattern Insync = ChangeTokenStatus' "INSYNC"

pattern Pending :: ChangeTokenStatus
pattern Pending = ChangeTokenStatus' "PENDING"

pattern Provisioned :: ChangeTokenStatus
pattern Provisioned = ChangeTokenStatus' "PROVISIONED"

{-# COMPLETE
  Insync,
  Pending,
  Provisioned,
  ChangeTokenStatus' #-}

instance FromText ChangeTokenStatus where
    parser = (ChangeTokenStatus' . mk) <$> takeText

instance ToText ChangeTokenStatus where
    toText (ChangeTokenStatus' ci) = original ci

-- | Represents an enum of /known/ $ChangeTokenStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ChangeTokenStatus where
    toEnum i = case i of
        0 -> Insync
        1 -> Pending
        2 -> Provisioned
        _ -> (error . showText) $ "Unknown index for ChangeTokenStatus: " <> toText i
    fromEnum x = case x of
        Insync -> 0
        Pending -> 1
        Provisioned -> 2
        ChangeTokenStatus' name -> (error . showText) $ "Unknown ChangeTokenStatus: " <> original name

-- | Represents the bounds of /known/ $ChangeTokenStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ChangeTokenStatus where
    minBound = Insync
    maxBound = Provisioned

instance Hashable     ChangeTokenStatus
instance NFData       ChangeTokenStatus
instance ToByteString ChangeTokenStatus
instance ToQuery      ChangeTokenStatus
instance ToHeader     ChangeTokenStatus

instance FromJSON ChangeTokenStatus where
    parseJSON = parseJSONText "ChangeTokenStatus"
