{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.SmoothGroupCertificateMode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.SmoothGroupCertificateMode (
  SmoothGroupCertificateMode (
    ..
    , SelfSigned
    , VerifyAuthenticity
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Smooth Group Certificate Mode
data SmoothGroupCertificateMode = SmoothGroupCertificateMode' (CI
                                                                 Text)
                                    deriving (Eq, Ord, Read, Show, Data,
                                              Typeable, Generic)

pattern SelfSigned :: SmoothGroupCertificateMode
pattern SelfSigned = SmoothGroupCertificateMode' "SELF_SIGNED"

pattern VerifyAuthenticity :: SmoothGroupCertificateMode
pattern VerifyAuthenticity = SmoothGroupCertificateMode' "VERIFY_AUTHENTICITY"

{-# COMPLETE
  SelfSigned,
  VerifyAuthenticity,
  SmoothGroupCertificateMode' #-}

instance FromText SmoothGroupCertificateMode where
    parser = (SmoothGroupCertificateMode' . mk) <$> takeText

instance ToText SmoothGroupCertificateMode where
    toText (SmoothGroupCertificateMode' ci) = original ci

-- | Represents an enum of /known/ $SmoothGroupCertificateMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum SmoothGroupCertificateMode where
    toEnum i = case i of
        0 -> SelfSigned
        1 -> VerifyAuthenticity
        _ -> (error . showText) $ "Unknown index for SmoothGroupCertificateMode: " <> toText i
    fromEnum x = case x of
        SelfSigned -> 0
        VerifyAuthenticity -> 1
        SmoothGroupCertificateMode' name -> (error . showText) $ "Unknown SmoothGroupCertificateMode: " <> original name

-- | Represents the bounds of /known/ $SmoothGroupCertificateMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded SmoothGroupCertificateMode where
    minBound = SelfSigned
    maxBound = VerifyAuthenticity

instance Hashable     SmoothGroupCertificateMode
instance NFData       SmoothGroupCertificateMode
instance ToByteString SmoothGroupCertificateMode
instance ToQuery      SmoothGroupCertificateMode
instance ToHeader     SmoothGroupCertificateMode

instance ToJSON SmoothGroupCertificateMode where
    toJSON = toJSONText

instance FromJSON SmoothGroupCertificateMode where
    parseJSON = parseJSONText "SmoothGroupCertificateMode"
