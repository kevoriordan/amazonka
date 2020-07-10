{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.PatchOperationType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.PatchOperationType (
  PatchOperationType (
    ..
    , Install
    , Scan
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data PatchOperationType = PatchOperationType' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern Install :: PatchOperationType
pattern Install = PatchOperationType' "Install"

pattern Scan :: PatchOperationType
pattern Scan = PatchOperationType' "Scan"

{-# COMPLETE
  Install,
  Scan,
  PatchOperationType' #-}

instance FromText PatchOperationType where
    parser = (PatchOperationType' . mk) <$> takeText

instance ToText PatchOperationType where
    toText (PatchOperationType' ci) = original ci

-- | Represents an enum of /known/ $PatchOperationType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum PatchOperationType where
    toEnum i = case i of
        0 -> Install
        1 -> Scan
        _ -> (error . showText) $ "Unknown index for PatchOperationType: " <> toText i
    fromEnum x = case x of
        Install -> 0
        Scan -> 1
        PatchOperationType' name -> (error . showText) $ "Unknown PatchOperationType: " <> original name

-- | Represents the bounds of /known/ $PatchOperationType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded PatchOperationType where
    minBound = Install
    maxBound = Scan

instance Hashable     PatchOperationType
instance NFData       PatchOperationType
instance ToByteString PatchOperationType
instance ToQuery      PatchOperationType
instance ToHeader     PatchOperationType

instance FromJSON PatchOperationType where
    parseJSON = parseJSONText "PatchOperationType"
