{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.CmafWriteHLSManifest
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.CmafWriteHLSManifest (
  CmafWriteHLSManifest (
    ..
    , CWHLSMDisabled
    , CWHLSMEnabled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | When set to ENABLED, an Apple HLS manifest will be generated for this output.
data CmafWriteHLSManifest = CmafWriteHLSManifest' (CI
                                                     Text)
                              deriving (Eq, Ord, Read, Show, Data, Typeable,
                                        Generic)

pattern CWHLSMDisabled :: CmafWriteHLSManifest
pattern CWHLSMDisabled = CmafWriteHLSManifest' "DISABLED"

pattern CWHLSMEnabled :: CmafWriteHLSManifest
pattern CWHLSMEnabled = CmafWriteHLSManifest' "ENABLED"

{-# COMPLETE
  CWHLSMDisabled,
  CWHLSMEnabled,
  CmafWriteHLSManifest' #-}

instance FromText CmafWriteHLSManifest where
    parser = (CmafWriteHLSManifest' . mk) <$> takeText

instance ToText CmafWriteHLSManifest where
    toText (CmafWriteHLSManifest' ci) = original ci

-- | Represents an enum of /known/ $CmafWriteHLSManifest.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum CmafWriteHLSManifest where
    toEnum i = case i of
        0 -> CWHLSMDisabled
        1 -> CWHLSMEnabled
        _ -> (error . showText) $ "Unknown index for CmafWriteHLSManifest: " <> toText i
    fromEnum x = case x of
        CWHLSMDisabled -> 0
        CWHLSMEnabled -> 1
        CmafWriteHLSManifest' name -> (error . showText) $ "Unknown CmafWriteHLSManifest: " <> original name

-- | Represents the bounds of /known/ $CmafWriteHLSManifest.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded CmafWriteHLSManifest where
    minBound = CWHLSMDisabled
    maxBound = CWHLSMEnabled

instance Hashable     CmafWriteHLSManifest
instance NFData       CmafWriteHLSManifest
instance ToByteString CmafWriteHLSManifest
instance ToQuery      CmafWriteHLSManifest
instance ToHeader     CmafWriteHLSManifest

instance ToJSON CmafWriteHLSManifest where
    toJSON = toJSONText

instance FromJSON CmafWriteHLSManifest where
    parseJSON = parseJSONText "CmafWriteHLSManifest"
