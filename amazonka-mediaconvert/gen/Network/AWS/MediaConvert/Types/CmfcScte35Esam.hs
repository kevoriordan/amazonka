{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.CmfcScte35Esam
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.CmfcScte35Esam (
  CmfcScte35Esam (
    ..
    , CSEInsert
    , CSENone
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Use this setting only when you specify SCTE-35 markers from ESAM. Choose INSERT to put SCTE-35 markers in this output at the insertion points that you specify in an ESAM XML document. Provide the document in the setting SCC XML (sccXml).
data CmfcScte35Esam = CmfcScte35Esam' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern CSEInsert :: CmfcScte35Esam
pattern CSEInsert = CmfcScte35Esam' "INSERT"

pattern CSENone :: CmfcScte35Esam
pattern CSENone = CmfcScte35Esam' "NONE"

{-# COMPLETE
  CSEInsert,
  CSENone,
  CmfcScte35Esam' #-}

instance FromText CmfcScte35Esam where
    parser = (CmfcScte35Esam' . mk) <$> takeText

instance ToText CmfcScte35Esam where
    toText (CmfcScte35Esam' ci) = original ci

-- | Represents an enum of /known/ $CmfcScte35Esam.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum CmfcScte35Esam where
    toEnum i = case i of
        0 -> CSEInsert
        1 -> CSENone
        _ -> (error . showText) $ "Unknown index for CmfcScte35Esam: " <> toText i
    fromEnum x = case x of
        CSEInsert -> 0
        CSENone -> 1
        CmfcScte35Esam' name -> (error . showText) $ "Unknown CmfcScte35Esam: " <> original name

-- | Represents the bounds of /known/ $CmfcScte35Esam.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded CmfcScte35Esam where
    minBound = CSEInsert
    maxBound = CSENone

instance Hashable     CmfcScte35Esam
instance NFData       CmfcScte35Esam
instance ToByteString CmfcScte35Esam
instance ToQuery      CmfcScte35Esam
instance ToHeader     CmfcScte35Esam

instance ToJSON CmfcScte35Esam where
    toJSON = toJSONText

instance FromJSON CmfcScte35Esam where
    parseJSON = parseJSONText "CmfcScte35Esam"
