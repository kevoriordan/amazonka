{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.FpgaImageStateCode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.FpgaImageStateCode (
  FpgaImageStateCode (
    ..
    , FISCAvailable
    , FISCFailed
    , FISCPending
    , FISCUnavailable
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data FpgaImageStateCode = FpgaImageStateCode' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern FISCAvailable :: FpgaImageStateCode
pattern FISCAvailable = FpgaImageStateCode' "available"

pattern FISCFailed :: FpgaImageStateCode
pattern FISCFailed = FpgaImageStateCode' "failed"

pattern FISCPending :: FpgaImageStateCode
pattern FISCPending = FpgaImageStateCode' "pending"

pattern FISCUnavailable :: FpgaImageStateCode
pattern FISCUnavailable = FpgaImageStateCode' "unavailable"

{-# COMPLETE
  FISCAvailable,
  FISCFailed,
  FISCPending,
  FISCUnavailable,
  FpgaImageStateCode' #-}

instance FromText FpgaImageStateCode where
    parser = (FpgaImageStateCode' . mk) <$> takeText

instance ToText FpgaImageStateCode where
    toText (FpgaImageStateCode' ci) = original ci

-- | Represents an enum of /known/ $FpgaImageStateCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum FpgaImageStateCode where
    toEnum i = case i of
        0 -> FISCAvailable
        1 -> FISCFailed
        2 -> FISCPending
        3 -> FISCUnavailable
        _ -> (error . showText) $ "Unknown index for FpgaImageStateCode: " <> toText i
    fromEnum x = case x of
        FISCAvailable -> 0
        FISCFailed -> 1
        FISCPending -> 2
        FISCUnavailable -> 3
        FpgaImageStateCode' name -> (error . showText) $ "Unknown FpgaImageStateCode: " <> original name

-- | Represents the bounds of /known/ $FpgaImageStateCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded FpgaImageStateCode where
    minBound = FISCAvailable
    maxBound = FISCUnavailable

instance Hashable     FpgaImageStateCode
instance NFData       FpgaImageStateCode
instance ToByteString FpgaImageStateCode
instance ToQuery      FpgaImageStateCode
instance ToHeader     FpgaImageStateCode

instance FromXML FpgaImageStateCode where
    parseXML = parseXMLText "FpgaImageStateCode"
