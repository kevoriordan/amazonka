{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.SipType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AlexaBusiness.Types.SipType (
  SipType (
    ..
    , Work
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data SipType = SipType' (CI Text)
                 deriving (Eq, Ord, Show, Data, Typeable, Generic)

pattern Work :: SipType
pattern Work = SipType' "WORK"

{-# COMPLETE
  Work,
  SipType' #-}

instance FromText SipType where
    parser = (SipType' . mk) <$> takeText

instance ToText SipType where
    toText (SipType' ci) = original ci

-- | Represents an enum of /known/ $SipType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum SipType where
    toEnum i = case i of
        0 -> Work
        _ -> (error . showText) $ "Unknown index for SipType: " <> toText i
    fromEnum x = case x of
        Work -> 0
        SipType' name -> (error . showText) $ "Unknown SipType: " <> original name

-- | Represents the bounds of /known/ $SipType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded SipType where
    minBound = Work
    maxBound = Work

instance Hashable     SipType
instance NFData       SipType
instance ToByteString SipType
instance ToQuery      SipType
instance ToHeader     SipType

instance ToJSON SipType where
    toJSON = toJSONText

instance FromJSON SipType where
    parseJSON = parseJSONText "SipType"
