{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ObjectStorageClass
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.ObjectStorageClass (
  ObjectStorageClass (
    ..
    , OSCGlacier
    , OSCIntelligentTiering
    , OSCReducedRedundancy
    , OSCStandard
    , OSCStandardIA
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude
import Network.AWS.S3.Internal

data ObjectStorageClass = ObjectStorageClass' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern OSCGlacier :: ObjectStorageClass
pattern OSCGlacier = ObjectStorageClass' "GLACIER"

pattern OSCIntelligentTiering :: ObjectStorageClass
pattern OSCIntelligentTiering = ObjectStorageClass' "INTELLIGENT_TIERING"

pattern OSCReducedRedundancy :: ObjectStorageClass
pattern OSCReducedRedundancy = ObjectStorageClass' "REDUCED_REDUNDANCY"

pattern OSCStandard :: ObjectStorageClass
pattern OSCStandard = ObjectStorageClass' "STANDARD"

pattern OSCStandardIA :: ObjectStorageClass
pattern OSCStandardIA = ObjectStorageClass' "STANDARD_IA"

{-# COMPLETE
  OSCGlacier,
  OSCIntelligentTiering,
  OSCReducedRedundancy,
  OSCStandard,
  OSCStandardIA,
  ObjectStorageClass' #-}

instance FromText ObjectStorageClass where
    parser = (ObjectStorageClass' . mk) <$> takeText

instance ToText ObjectStorageClass where
    toText (ObjectStorageClass' ci) = original ci

-- | Represents an enum of /known/ $ObjectStorageClass.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ObjectStorageClass where
    toEnum i = case i of
        0 -> OSCGlacier
        1 -> OSCIntelligentTiering
        2 -> OSCReducedRedundancy
        3 -> OSCStandard
        4 -> OSCStandardIA
        _ -> (error . showText) $ "Unknown index for ObjectStorageClass: " <> toText i
    fromEnum x = case x of
        OSCGlacier -> 0
        OSCIntelligentTiering -> 1
        OSCReducedRedundancy -> 2
        OSCStandard -> 3
        OSCStandardIA -> 4
        ObjectStorageClass' name -> (error . showText) $ "Unknown ObjectStorageClass: " <> original name

-- | Represents the bounds of /known/ $ObjectStorageClass.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ObjectStorageClass where
    minBound = OSCGlacier
    maxBound = OSCStandardIA

instance Hashable     ObjectStorageClass
instance NFData       ObjectStorageClass
instance ToByteString ObjectStorageClass
instance ToQuery      ObjectStorageClass
instance ToHeader     ObjectStorageClass

instance FromXML ObjectStorageClass where
    parseXML = parseXMLText "ObjectStorageClass"
