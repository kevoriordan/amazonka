{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.Types.ChangeType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DAX.Types.ChangeType (
  ChangeType (
    ..
    , Immediate
    , RequiresReboot
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ChangeType = ChangeType' (CI Text)
                    deriving (Eq, Ord, Read, Show, Data, Typeable,
                              Generic)

pattern Immediate :: ChangeType
pattern Immediate = ChangeType' "IMMEDIATE"

pattern RequiresReboot :: ChangeType
pattern RequiresReboot = ChangeType' "REQUIRES_REBOOT"

{-# COMPLETE
  Immediate,
  RequiresReboot,
  ChangeType' #-}

instance FromText ChangeType where
    parser = (ChangeType' . mk) <$> takeText

instance ToText ChangeType where
    toText (ChangeType' ci) = original ci

-- | Represents an enum of /known/ $ChangeType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ChangeType where
    toEnum i = case i of
        0 -> Immediate
        1 -> RequiresReboot
        _ -> (error . showText) $ "Unknown index for ChangeType: " <> toText i
    fromEnum x = case x of
        Immediate -> 0
        RequiresReboot -> 1
        ChangeType' name -> (error . showText) $ "Unknown ChangeType: " <> original name

-- | Represents the bounds of /known/ $ChangeType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ChangeType where
    minBound = Immediate
    maxBound = RequiresReboot

instance Hashable     ChangeType
instance NFData       ChangeType
instance ToByteString ChangeType
instance ToQuery      ChangeType
instance ToHeader     ChangeType

instance FromJSON ChangeType where
    parseJSON = parseJSONText "ChangeType"
