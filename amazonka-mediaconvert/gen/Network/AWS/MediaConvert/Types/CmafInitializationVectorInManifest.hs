{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.CmafInitializationVectorInManifest
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.CmafInitializationVectorInManifest (
  CmafInitializationVectorInManifest (
    ..
    , CIVIMExclude
    , CIVIMInclude
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | When you use DRM with CMAF outputs, choose whether the service writes the 128-bit encryption initialization vector in the HLS and DASH manifests.
data CmafInitializationVectorInManifest = CmafInitializationVectorInManifest' (CI
                                                                                 Text)
                                            deriving (Eq, Ord, Read, Show, Data,
                                                      Typeable, Generic)

pattern CIVIMExclude :: CmafInitializationVectorInManifest
pattern CIVIMExclude = CmafInitializationVectorInManifest' "EXCLUDE"

pattern CIVIMInclude :: CmafInitializationVectorInManifest
pattern CIVIMInclude = CmafInitializationVectorInManifest' "INCLUDE"

{-# COMPLETE
  CIVIMExclude,
  CIVIMInclude,
  CmafInitializationVectorInManifest' #-}

instance FromText CmafInitializationVectorInManifest where
    parser = (CmafInitializationVectorInManifest' . mk) <$> takeText

instance ToText CmafInitializationVectorInManifest where
    toText (CmafInitializationVectorInManifest' ci) = original ci

-- | Represents an enum of /known/ $CmafInitializationVectorInManifest.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum CmafInitializationVectorInManifest where
    toEnum i = case i of
        0 -> CIVIMExclude
        1 -> CIVIMInclude
        _ -> (error . showText) $ "Unknown index for CmafInitializationVectorInManifest: " <> toText i
    fromEnum x = case x of
        CIVIMExclude -> 0
        CIVIMInclude -> 1
        CmafInitializationVectorInManifest' name -> (error . showText) $ "Unknown CmafInitializationVectorInManifest: " <> original name

-- | Represents the bounds of /known/ $CmafInitializationVectorInManifest.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded CmafInitializationVectorInManifest where
    minBound = CIVIMExclude
    maxBound = CIVIMInclude

instance Hashable     CmafInitializationVectorInManifest
instance NFData       CmafInitializationVectorInManifest
instance ToByteString CmafInitializationVectorInManifest
instance ToQuery      CmafInitializationVectorInManifest
instance ToHeader     CmafInitializationVectorInManifest

instance ToJSON CmafInitializationVectorInManifest where
    toJSON = toJSONText

instance FromJSON CmafInitializationVectorInManifest where
    parseJSON = parseJSONText "CmafInitializationVectorInManifest"
