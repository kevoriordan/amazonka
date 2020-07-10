{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.TargetLabel
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeDeploy.Types.TargetLabel (
  TargetLabel (
    ..
    , TLBlue
    , TLGreen
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data TargetLabel = TargetLabel' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern TLBlue :: TargetLabel
pattern TLBlue = TargetLabel' "Blue"

pattern TLGreen :: TargetLabel
pattern TLGreen = TargetLabel' "Green"

{-# COMPLETE
  TLBlue,
  TLGreen,
  TargetLabel' #-}

instance FromText TargetLabel where
    parser = (TargetLabel' . mk) <$> takeText

instance ToText TargetLabel where
    toText (TargetLabel' ci) = original ci

-- | Represents an enum of /known/ $TargetLabel.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum TargetLabel where
    toEnum i = case i of
        0 -> TLBlue
        1 -> TLGreen
        _ -> (error . showText) $ "Unknown index for TargetLabel: " <> toText i
    fromEnum x = case x of
        TLBlue -> 0
        TLGreen -> 1
        TargetLabel' name -> (error . showText) $ "Unknown TargetLabel: " <> original name

-- | Represents the bounds of /known/ $TargetLabel.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded TargetLabel where
    minBound = TLBlue
    maxBound = TLGreen

instance Hashable     TargetLabel
instance NFData       TargetLabel
instance ToByteString TargetLabel
instance ToQuery      TargetLabel
instance ToHeader     TargetLabel

instance FromJSON TargetLabel where
    parseJSON = parseJSONText "TargetLabel"
