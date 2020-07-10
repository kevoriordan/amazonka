{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.HlsInitializationVectorInManifest
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.HlsInitializationVectorInManifest (
  HlsInitializationVectorInManifest (
    ..
    , HIVIMExclude
    , HIVIMInclude
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | The Initialization Vector is a 128-bit number used in conjunction with the key for encrypting blocks. If set to INCLUDE, Initialization Vector is listed in the manifest. Otherwise Initialization Vector is not in the manifest.
data HlsInitializationVectorInManifest = HlsInitializationVectorInManifest' (CI
                                                                               Text)
                                           deriving (Eq, Ord, Read, Show, Data,
                                                     Typeable, Generic)

pattern HIVIMExclude :: HlsInitializationVectorInManifest
pattern HIVIMExclude = HlsInitializationVectorInManifest' "EXCLUDE"

pattern HIVIMInclude :: HlsInitializationVectorInManifest
pattern HIVIMInclude = HlsInitializationVectorInManifest' "INCLUDE"

{-# COMPLETE
  HIVIMExclude,
  HIVIMInclude,
  HlsInitializationVectorInManifest' #-}

instance FromText HlsInitializationVectorInManifest where
    parser = (HlsInitializationVectorInManifest' . mk) <$> takeText

instance ToText HlsInitializationVectorInManifest where
    toText (HlsInitializationVectorInManifest' ci) = original ci

-- | Represents an enum of /known/ $HlsInitializationVectorInManifest.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum HlsInitializationVectorInManifest where
    toEnum i = case i of
        0 -> HIVIMExclude
        1 -> HIVIMInclude
        _ -> (error . showText) $ "Unknown index for HlsInitializationVectorInManifest: " <> toText i
    fromEnum x = case x of
        HIVIMExclude -> 0
        HIVIMInclude -> 1
        HlsInitializationVectorInManifest' name -> (error . showText) $ "Unknown HlsInitializationVectorInManifest: " <> original name

-- | Represents the bounds of /known/ $HlsInitializationVectorInManifest.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded HlsInitializationVectorInManifest where
    minBound = HIVIMExclude
    maxBound = HIVIMInclude

instance Hashable     HlsInitializationVectorInManifest
instance NFData       HlsInitializationVectorInManifest
instance ToByteString HlsInitializationVectorInManifest
instance ToQuery      HlsInitializationVectorInManifest
instance ToHeader     HlsInitializationVectorInManifest

instance ToJSON HlsInitializationVectorInManifest where
    toJSON = toJSONText

instance FromJSON HlsInitializationVectorInManifest where
    parseJSON = parseJSONText "HlsInitializationVectorInManifest"
