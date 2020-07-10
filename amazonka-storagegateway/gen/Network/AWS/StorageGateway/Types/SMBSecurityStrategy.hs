{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.SMBSecurityStrategy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.StorageGateway.Types.SMBSecurityStrategy (
  SMBSecurityStrategy (
    ..
    , ClientSpecified
    , MandatoryEncryption
    , MandatorySigning
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data SMBSecurityStrategy = SMBSecurityStrategy' (CI
                                                   Text)
                             deriving (Eq, Ord, Read, Show, Data, Typeable,
                                       Generic)

pattern ClientSpecified :: SMBSecurityStrategy
pattern ClientSpecified = SMBSecurityStrategy' "ClientSpecified"

pattern MandatoryEncryption :: SMBSecurityStrategy
pattern MandatoryEncryption = SMBSecurityStrategy' "MandatoryEncryption"

pattern MandatorySigning :: SMBSecurityStrategy
pattern MandatorySigning = SMBSecurityStrategy' "MandatorySigning"

{-# COMPLETE
  ClientSpecified,
  MandatoryEncryption,
  MandatorySigning,
  SMBSecurityStrategy' #-}

instance FromText SMBSecurityStrategy where
    parser = (SMBSecurityStrategy' . mk) <$> takeText

instance ToText SMBSecurityStrategy where
    toText (SMBSecurityStrategy' ci) = original ci

-- | Represents an enum of /known/ $SMBSecurityStrategy.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum SMBSecurityStrategy where
    toEnum i = case i of
        0 -> ClientSpecified
        1 -> MandatoryEncryption
        2 -> MandatorySigning
        _ -> (error . showText) $ "Unknown index for SMBSecurityStrategy: " <> toText i
    fromEnum x = case x of
        ClientSpecified -> 0
        MandatoryEncryption -> 1
        MandatorySigning -> 2
        SMBSecurityStrategy' name -> (error . showText) $ "Unknown SMBSecurityStrategy: " <> original name

-- | Represents the bounds of /known/ $SMBSecurityStrategy.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded SMBSecurityStrategy where
    minBound = ClientSpecified
    maxBound = MandatorySigning

instance Hashable     SMBSecurityStrategy
instance NFData       SMBSecurityStrategy
instance ToByteString SMBSecurityStrategy
instance ToQuery      SMBSecurityStrategy
instance ToHeader     SMBSecurityStrategy

instance ToJSON SMBSecurityStrategy where
    toJSON = toJSONText

instance FromJSON SMBSecurityStrategy where
    parseJSON = parseJSONText "SMBSecurityStrategy"
