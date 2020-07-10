{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.CmafWriteDASHManifest
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.CmafWriteDASHManifest (
  CmafWriteDASHManifest (
    ..
    , CWDASHMDisabled
    , CWDASHMEnabled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | When set to ENABLED, a DASH MPD manifest will be generated for this output.
data CmafWriteDASHManifest = CmafWriteDASHManifest' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern CWDASHMDisabled :: CmafWriteDASHManifest
pattern CWDASHMDisabled = CmafWriteDASHManifest' "DISABLED"

pattern CWDASHMEnabled :: CmafWriteDASHManifest
pattern CWDASHMEnabled = CmafWriteDASHManifest' "ENABLED"

{-# COMPLETE
  CWDASHMDisabled,
  CWDASHMEnabled,
  CmafWriteDASHManifest' #-}

instance FromText CmafWriteDASHManifest where
    parser = (CmafWriteDASHManifest' . mk) <$> takeText

instance ToText CmafWriteDASHManifest where
    toText (CmafWriteDASHManifest' ci) = original ci

-- | Represents an enum of /known/ $CmafWriteDASHManifest.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum CmafWriteDASHManifest where
    toEnum i = case i of
        0 -> CWDASHMDisabled
        1 -> CWDASHMEnabled
        _ -> (error . showText) $ "Unknown index for CmafWriteDASHManifest: " <> toText i
    fromEnum x = case x of
        CWDASHMDisabled -> 0
        CWDASHMEnabled -> 1
        CmafWriteDASHManifest' name -> (error . showText) $ "Unknown CmafWriteDASHManifest: " <> original name

-- | Represents the bounds of /known/ $CmafWriteDASHManifest.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded CmafWriteDASHManifest where
    minBound = CWDASHMDisabled
    maxBound = CWDASHMEnabled

instance Hashable     CmafWriteDASHManifest
instance NFData       CmafWriteDASHManifest
instance ToByteString CmafWriteDASHManifest
instance ToQuery      CmafWriteDASHManifest
instance ToHeader     CmafWriteDASHManifest

instance ToJSON CmafWriteDASHManifest where
    toJSON = toJSONText

instance FromJSON CmafWriteDASHManifest where
    parseJSON = parseJSONText "CmafWriteDASHManifest"
