{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H264Syntax
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.H264Syntax (
  H264Syntax (
    ..
    , HSDefault
    , HSRP2027
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Produces a bitstream compliant with SMPTE RP-2027.
data H264Syntax = H264Syntax' (CI Text)
                    deriving (Eq, Ord, Read, Show, Data, Typeable,
                              Generic)

pattern HSDefault :: H264Syntax
pattern HSDefault = H264Syntax' "DEFAULT"

pattern HSRP2027 :: H264Syntax
pattern HSRP2027 = H264Syntax' "RP2027"

{-# COMPLETE
  HSDefault,
  HSRP2027,
  H264Syntax' #-}

instance FromText H264Syntax where
    parser = (H264Syntax' . mk) <$> takeText

instance ToText H264Syntax where
    toText (H264Syntax' ci) = original ci

-- | Represents an enum of /known/ $H264Syntax.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum H264Syntax where
    toEnum i = case i of
        0 -> HSDefault
        1 -> HSRP2027
        _ -> (error . showText) $ "Unknown index for H264Syntax: " <> toText i
    fromEnum x = case x of
        HSDefault -> 0
        HSRP2027 -> 1
        H264Syntax' name -> (error . showText) $ "Unknown H264Syntax: " <> original name

-- | Represents the bounds of /known/ $H264Syntax.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded H264Syntax where
    minBound = HSDefault
    maxBound = HSRP2027

instance Hashable     H264Syntax
instance NFData       H264Syntax
instance ToByteString H264Syntax
instance ToQuery      H264Syntax
instance ToHeader     H264Syntax

instance ToJSON H264Syntax where
    toJSON = toJSONText

instance FromJSON H264Syntax where
    parseJSON = parseJSONText "H264Syntax"
