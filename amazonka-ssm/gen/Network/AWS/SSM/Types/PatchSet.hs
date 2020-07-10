{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.PatchSet
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.PatchSet (
  PatchSet (
    ..
    , Application
    , OS
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data PatchSet = PatchSet' (CI Text)
                  deriving (Eq, Ord, Read, Show, Data, Typeable,
                            Generic)

pattern Application :: PatchSet
pattern Application = PatchSet' "APPLICATION"

pattern OS :: PatchSet
pattern OS = PatchSet' "OS"

{-# COMPLETE
  Application,
  OS,
  PatchSet' #-}

instance FromText PatchSet where
    parser = (PatchSet' . mk) <$> takeText

instance ToText PatchSet where
    toText (PatchSet' ci) = original ci

-- | Represents an enum of /known/ $PatchSet.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum PatchSet where
    toEnum i = case i of
        0 -> Application
        1 -> OS
        _ -> (error . showText) $ "Unknown index for PatchSet: " <> toText i
    fromEnum x = case x of
        Application -> 0
        OS -> 1
        PatchSet' name -> (error . showText) $ "Unknown PatchSet: " <> original name

-- | Represents the bounds of /known/ $PatchSet.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded PatchSet where
    minBound = Application
    maxBound = OS

instance Hashable     PatchSet
instance NFData       PatchSet
instance ToByteString PatchSet
instance ToQuery      PatchSet
instance ToHeader     PatchSet

instance ToJSON PatchSet where
    toJSON = toJSONText
