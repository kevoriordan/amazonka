{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.PatchFilterKey
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.PatchFilterKey (
  PatchFilterKey (
    ..
    , Classification
    , MsrcSeverity
    , PatchId
    , PatchSet
    , Priority
    , Product
    , ProductFamily
    , Section
    , Severity
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data PatchFilterKey = PatchFilterKey' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern Classification :: PatchFilterKey
pattern Classification = PatchFilterKey' "CLASSIFICATION"

pattern MsrcSeverity :: PatchFilterKey
pattern MsrcSeverity = PatchFilterKey' "MSRC_SEVERITY"

pattern PatchId :: PatchFilterKey
pattern PatchId = PatchFilterKey' "PATCH_ID"

pattern PatchSet :: PatchFilterKey
pattern PatchSet = PatchFilterKey' "PATCH_SET"

pattern Priority :: PatchFilterKey
pattern Priority = PatchFilterKey' "PRIORITY"

pattern Product :: PatchFilterKey
pattern Product = PatchFilterKey' "PRODUCT"

pattern ProductFamily :: PatchFilterKey
pattern ProductFamily = PatchFilterKey' "PRODUCT_FAMILY"

pattern Section :: PatchFilterKey
pattern Section = PatchFilterKey' "SECTION"

pattern Severity :: PatchFilterKey
pattern Severity = PatchFilterKey' "SEVERITY"

{-# COMPLETE
  Classification,
  MsrcSeverity,
  PatchId,
  PatchSet,
  Priority,
  Product,
  ProductFamily,
  Section,
  Severity,
  PatchFilterKey' #-}

instance FromText PatchFilterKey where
    parser = (PatchFilterKey' . mk) <$> takeText

instance ToText PatchFilterKey where
    toText (PatchFilterKey' ci) = original ci

-- | Represents an enum of /known/ $PatchFilterKey.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum PatchFilterKey where
    toEnum i = case i of
        0 -> Classification
        1 -> MsrcSeverity
        2 -> PatchId
        3 -> PatchSet
        4 -> Priority
        5 -> Product
        6 -> ProductFamily
        7 -> Section
        8 -> Severity
        _ -> (error . showText) $ "Unknown index for PatchFilterKey: " <> toText i
    fromEnum x = case x of
        Classification -> 0
        MsrcSeverity -> 1
        PatchId -> 2
        PatchSet -> 3
        Priority -> 4
        Product -> 5
        ProductFamily -> 6
        Section -> 7
        Severity -> 8
        PatchFilterKey' name -> (error . showText) $ "Unknown PatchFilterKey: " <> original name

-- | Represents the bounds of /known/ $PatchFilterKey.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded PatchFilterKey where
    minBound = Classification
    maxBound = Severity

instance Hashable     PatchFilterKey
instance NFData       PatchFilterKey
instance ToByteString PatchFilterKey
instance ToQuery      PatchFilterKey
instance ToHeader     PatchFilterKey

instance ToJSON PatchFilterKey where
    toJSON = toJSONText

instance FromJSON PatchFilterKey where
    parseJSON = parseJSONText "PatchFilterKey"
