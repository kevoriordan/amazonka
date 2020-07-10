{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.PatchProperty
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.PatchProperty (
  PatchProperty (
    ..
    , PPClassification
    , PPMsrcSeverity
    , PPPriority
    , PPProduct
    , PPProductFamily
    , PPSeverity
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data PatchProperty = PatchProperty' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern PPClassification :: PatchProperty
pattern PPClassification = PatchProperty' "CLASSIFICATION"

pattern PPMsrcSeverity :: PatchProperty
pattern PPMsrcSeverity = PatchProperty' "MSRC_SEVERITY"

pattern PPPriority :: PatchProperty
pattern PPPriority = PatchProperty' "PRIORITY"

pattern PPProduct :: PatchProperty
pattern PPProduct = PatchProperty' "PRODUCT"

pattern PPProductFamily :: PatchProperty
pattern PPProductFamily = PatchProperty' "PRODUCT_FAMILY"

pattern PPSeverity :: PatchProperty
pattern PPSeverity = PatchProperty' "SEVERITY"

{-# COMPLETE
  PPClassification,
  PPMsrcSeverity,
  PPPriority,
  PPProduct,
  PPProductFamily,
  PPSeverity,
  PatchProperty' #-}

instance FromText PatchProperty where
    parser = (PatchProperty' . mk) <$> takeText

instance ToText PatchProperty where
    toText (PatchProperty' ci) = original ci

-- | Represents an enum of /known/ $PatchProperty.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum PatchProperty where
    toEnum i = case i of
        0 -> PPClassification
        1 -> PPMsrcSeverity
        2 -> PPPriority
        3 -> PPProduct
        4 -> PPProductFamily
        5 -> PPSeverity
        _ -> (error . showText) $ "Unknown index for PatchProperty: " <> toText i
    fromEnum x = case x of
        PPClassification -> 0
        PPMsrcSeverity -> 1
        PPPriority -> 2
        PPProduct -> 3
        PPProductFamily -> 4
        PPSeverity -> 5
        PatchProperty' name -> (error . showText) $ "Unknown PatchProperty: " <> original name

-- | Represents the bounds of /known/ $PatchProperty.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded PatchProperty where
    minBound = PPClassification
    maxBound = PPSeverity

instance Hashable     PatchProperty
instance NFData       PatchProperty
instance ToByteString PatchProperty
instance ToQuery      PatchProperty
instance ToHeader     PatchProperty

instance ToJSON PatchProperty where
    toJSON = toJSONText
