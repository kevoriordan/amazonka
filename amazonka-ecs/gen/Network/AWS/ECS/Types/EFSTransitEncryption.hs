{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.EFSTransitEncryption
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECS.Types.EFSTransitEncryption (
  EFSTransitEncryption (
    ..
    , EFSTEDisabled
    , EFSTEEnabled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data EFSTransitEncryption = EFSTransitEncryption' (CI
                                                     Text)
                              deriving (Eq, Ord, Read, Show, Data, Typeable,
                                        Generic)

pattern EFSTEDisabled :: EFSTransitEncryption
pattern EFSTEDisabled = EFSTransitEncryption' "DISABLED"

pattern EFSTEEnabled :: EFSTransitEncryption
pattern EFSTEEnabled = EFSTransitEncryption' "ENABLED"

{-# COMPLETE
  EFSTEDisabled,
  EFSTEEnabled,
  EFSTransitEncryption' #-}

instance FromText EFSTransitEncryption where
    parser = (EFSTransitEncryption' . mk) <$> takeText

instance ToText EFSTransitEncryption where
    toText (EFSTransitEncryption' ci) = original ci

-- | Represents an enum of /known/ $EFSTransitEncryption.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum EFSTransitEncryption where
    toEnum i = case i of
        0 -> EFSTEDisabled
        1 -> EFSTEEnabled
        _ -> (error . showText) $ "Unknown index for EFSTransitEncryption: " <> toText i
    fromEnum x = case x of
        EFSTEDisabled -> 0
        EFSTEEnabled -> 1
        EFSTransitEncryption' name -> (error . showText) $ "Unknown EFSTransitEncryption: " <> original name

-- | Represents the bounds of /known/ $EFSTransitEncryption.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded EFSTransitEncryption where
    minBound = EFSTEDisabled
    maxBound = EFSTEEnabled

instance Hashable     EFSTransitEncryption
instance NFData       EFSTransitEncryption
instance ToByteString EFSTransitEncryption
instance ToQuery      EFSTransitEncryption
instance ToHeader     EFSTransitEncryption

instance ToJSON EFSTransitEncryption where
    toJSON = toJSONText

instance FromJSON EFSTransitEncryption where
    parseJSON = parseJSONText "EFSTransitEncryption"
