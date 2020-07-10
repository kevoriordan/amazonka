{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.ApplyMethod
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.RDS.Types.ApplyMethod (
  ApplyMethod (
    ..
    , Immediate
    , PendingReboot
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ApplyMethod = ApplyMethod' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern Immediate :: ApplyMethod
pattern Immediate = ApplyMethod' "immediate"

pattern PendingReboot :: ApplyMethod
pattern PendingReboot = ApplyMethod' "pending-reboot"

{-# COMPLETE
  Immediate,
  PendingReboot,
  ApplyMethod' #-}

instance FromText ApplyMethod where
    parser = (ApplyMethod' . mk) <$> takeText

instance ToText ApplyMethod where
    toText (ApplyMethod' ci) = original ci

-- | Represents an enum of /known/ $ApplyMethod.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ApplyMethod where
    toEnum i = case i of
        0 -> Immediate
        1 -> PendingReboot
        _ -> (error . showText) $ "Unknown index for ApplyMethod: " <> toText i
    fromEnum x = case x of
        Immediate -> 0
        PendingReboot -> 1
        ApplyMethod' name -> (error . showText) $ "Unknown ApplyMethod: " <> original name

-- | Represents the bounds of /known/ $ApplyMethod.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ApplyMethod where
    minBound = Immediate
    maxBound = PendingReboot

instance Hashable     ApplyMethod
instance NFData       ApplyMethod
instance ToByteString ApplyMethod
instance ToQuery      ApplyMethod
instance ToHeader     ApplyMethod

instance FromXML ApplyMethod where
    parseXML = parseXMLText "ApplyMethod"
