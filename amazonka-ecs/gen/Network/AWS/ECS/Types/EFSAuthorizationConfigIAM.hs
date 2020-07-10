{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.EFSAuthorizationConfigIAM
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECS.Types.EFSAuthorizationConfigIAM (
  EFSAuthorizationConfigIAM (
    ..
    , EFSACIDisabled
    , EFSACIEnabled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data EFSAuthorizationConfigIAM = EFSAuthorizationConfigIAM' (CI
                                                               Text)
                                   deriving (Eq, Ord, Read, Show, Data,
                                             Typeable, Generic)

pattern EFSACIDisabled :: EFSAuthorizationConfigIAM
pattern EFSACIDisabled = EFSAuthorizationConfigIAM' "DISABLED"

pattern EFSACIEnabled :: EFSAuthorizationConfigIAM
pattern EFSACIEnabled = EFSAuthorizationConfigIAM' "ENABLED"

{-# COMPLETE
  EFSACIDisabled,
  EFSACIEnabled,
  EFSAuthorizationConfigIAM' #-}

instance FromText EFSAuthorizationConfigIAM where
    parser = (EFSAuthorizationConfigIAM' . mk) <$> takeText

instance ToText EFSAuthorizationConfigIAM where
    toText (EFSAuthorizationConfigIAM' ci) = original ci

-- | Represents an enum of /known/ $EFSAuthorizationConfigIAM.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum EFSAuthorizationConfigIAM where
    toEnum i = case i of
        0 -> EFSACIDisabled
        1 -> EFSACIEnabled
        _ -> (error . showText) $ "Unknown index for EFSAuthorizationConfigIAM: " <> toText i
    fromEnum x = case x of
        EFSACIDisabled -> 0
        EFSACIEnabled -> 1
        EFSAuthorizationConfigIAM' name -> (error . showText) $ "Unknown EFSAuthorizationConfigIAM: " <> original name

-- | Represents the bounds of /known/ $EFSAuthorizationConfigIAM.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded EFSAuthorizationConfigIAM where
    minBound = EFSACIDisabled
    maxBound = EFSACIEnabled

instance Hashable     EFSAuthorizationConfigIAM
instance NFData       EFSAuthorizationConfigIAM
instance ToByteString EFSAuthorizationConfigIAM
instance ToQuery      EFSAuthorizationConfigIAM
instance ToHeader     EFSAuthorizationConfigIAM

instance ToJSON EFSAuthorizationConfigIAM where
    toJSON = toJSONText

instance FromJSON EFSAuthorizationConfigIAM where
    parseJSON = parseJSONText "EFSAuthorizationConfigIAM"
