{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.BurnInTeletextGridControl
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.BurnInTeletextGridControl (
  BurnInTeletextGridControl (
    ..
    , BITGCFixed
    , BITGCScaled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Burn In Teletext Grid Control
data BurnInTeletextGridControl = BurnInTeletextGridControl' (CI
                                                               Text)
                                   deriving (Eq, Ord, Read, Show, Data,
                                             Typeable, Generic)

pattern BITGCFixed :: BurnInTeletextGridControl
pattern BITGCFixed = BurnInTeletextGridControl' "FIXED"

pattern BITGCScaled :: BurnInTeletextGridControl
pattern BITGCScaled = BurnInTeletextGridControl' "SCALED"

{-# COMPLETE
  BITGCFixed,
  BITGCScaled,
  BurnInTeletextGridControl' #-}

instance FromText BurnInTeletextGridControl where
    parser = (BurnInTeletextGridControl' . mk) <$> takeText

instance ToText BurnInTeletextGridControl where
    toText (BurnInTeletextGridControl' ci) = original ci

-- | Represents an enum of /known/ $BurnInTeletextGridControl.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum BurnInTeletextGridControl where
    toEnum i = case i of
        0 -> BITGCFixed
        1 -> BITGCScaled
        _ -> (error . showText) $ "Unknown index for BurnInTeletextGridControl: " <> toText i
    fromEnum x = case x of
        BITGCFixed -> 0
        BITGCScaled -> 1
        BurnInTeletextGridControl' name -> (error . showText) $ "Unknown BurnInTeletextGridControl: " <> original name

-- | Represents the bounds of /known/ $BurnInTeletextGridControl.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded BurnInTeletextGridControl where
    minBound = BITGCFixed
    maxBound = BITGCScaled

instance Hashable     BurnInTeletextGridControl
instance NFData       BurnInTeletextGridControl
instance ToByteString BurnInTeletextGridControl
instance ToQuery      BurnInTeletextGridControl
instance ToHeader     BurnInTeletextGridControl

instance ToJSON BurnInTeletextGridControl where
    toJSON = toJSONText

instance FromJSON BurnInTeletextGridControl where
    parseJSON = parseJSONText "BurnInTeletextGridControl"
