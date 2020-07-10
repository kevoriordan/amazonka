{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.ViewerProtocolPolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types.ViewerProtocolPolicy (
  ViewerProtocolPolicy (
    ..
    , VPPAllowAll
    , VPPHTTPSOnly
    , VPPRedirectToHTTPS
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ViewerProtocolPolicy = ViewerProtocolPolicy' (CI
                                                     Text)
                              deriving (Eq, Ord, Read, Show, Data, Typeable,
                                        Generic)

pattern VPPAllowAll :: ViewerProtocolPolicy
pattern VPPAllowAll = ViewerProtocolPolicy' "allow-all"

pattern VPPHTTPSOnly :: ViewerProtocolPolicy
pattern VPPHTTPSOnly = ViewerProtocolPolicy' "https-only"

pattern VPPRedirectToHTTPS :: ViewerProtocolPolicy
pattern VPPRedirectToHTTPS = ViewerProtocolPolicy' "redirect-to-https"

{-# COMPLETE
  VPPAllowAll,
  VPPHTTPSOnly,
  VPPRedirectToHTTPS,
  ViewerProtocolPolicy' #-}

instance FromText ViewerProtocolPolicy where
    parser = (ViewerProtocolPolicy' . mk) <$> takeText

instance ToText ViewerProtocolPolicy where
    toText (ViewerProtocolPolicy' ci) = original ci

-- | Represents an enum of /known/ $ViewerProtocolPolicy.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ViewerProtocolPolicy where
    toEnum i = case i of
        0 -> VPPAllowAll
        1 -> VPPHTTPSOnly
        2 -> VPPRedirectToHTTPS
        _ -> (error . showText) $ "Unknown index for ViewerProtocolPolicy: " <> toText i
    fromEnum x = case x of
        VPPAllowAll -> 0
        VPPHTTPSOnly -> 1
        VPPRedirectToHTTPS -> 2
        ViewerProtocolPolicy' name -> (error . showText) $ "Unknown ViewerProtocolPolicy: " <> original name

-- | Represents the bounds of /known/ $ViewerProtocolPolicy.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ViewerProtocolPolicy where
    minBound = VPPAllowAll
    maxBound = VPPRedirectToHTTPS

instance Hashable     ViewerProtocolPolicy
instance NFData       ViewerProtocolPolicy
instance ToByteString ViewerProtocolPolicy
instance ToQuery      ViewerProtocolPolicy
instance ToHeader     ViewerProtocolPolicy

instance FromXML ViewerProtocolPolicy where
    parseXML = parseXMLText "ViewerProtocolPolicy"

instance ToXML ViewerProtocolPolicy where
    toXML = toXMLText
