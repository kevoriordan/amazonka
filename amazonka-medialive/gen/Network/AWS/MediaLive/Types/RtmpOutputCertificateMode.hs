{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.RtmpOutputCertificateMode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.RtmpOutputCertificateMode (
  RtmpOutputCertificateMode (
    ..
    , ROCMSelfSigned
    , ROCMVerifyAuthenticity
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Rtmp Output Certificate Mode
data RtmpOutputCertificateMode = RtmpOutputCertificateMode' (CI
                                                               Text)
                                   deriving (Eq, Ord, Read, Show, Data,
                                             Typeable, Generic)

pattern ROCMSelfSigned :: RtmpOutputCertificateMode
pattern ROCMSelfSigned = RtmpOutputCertificateMode' "SELF_SIGNED"

pattern ROCMVerifyAuthenticity :: RtmpOutputCertificateMode
pattern ROCMVerifyAuthenticity = RtmpOutputCertificateMode' "VERIFY_AUTHENTICITY"

{-# COMPLETE
  ROCMSelfSigned,
  ROCMVerifyAuthenticity,
  RtmpOutputCertificateMode' #-}

instance FromText RtmpOutputCertificateMode where
    parser = (RtmpOutputCertificateMode' . mk) <$> takeText

instance ToText RtmpOutputCertificateMode where
    toText (RtmpOutputCertificateMode' ci) = original ci

-- | Represents an enum of /known/ $RtmpOutputCertificateMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum RtmpOutputCertificateMode where
    toEnum i = case i of
        0 -> ROCMSelfSigned
        1 -> ROCMVerifyAuthenticity
        _ -> (error . showText) $ "Unknown index for RtmpOutputCertificateMode: " <> toText i
    fromEnum x = case x of
        ROCMSelfSigned -> 0
        ROCMVerifyAuthenticity -> 1
        RtmpOutputCertificateMode' name -> (error . showText) $ "Unknown RtmpOutputCertificateMode: " <> original name

-- | Represents the bounds of /known/ $RtmpOutputCertificateMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded RtmpOutputCertificateMode where
    minBound = ROCMSelfSigned
    maxBound = ROCMVerifyAuthenticity

instance Hashable     RtmpOutputCertificateMode
instance NFData       RtmpOutputCertificateMode
instance ToByteString RtmpOutputCertificateMode
instance ToQuery      RtmpOutputCertificateMode
instance ToHeader     RtmpOutputCertificateMode

instance ToJSON RtmpOutputCertificateMode where
    toJSON = toJSONText

instance FromJSON RtmpOutputCertificateMode where
    parseJSON = parseJSONText "RtmpOutputCertificateMode"
