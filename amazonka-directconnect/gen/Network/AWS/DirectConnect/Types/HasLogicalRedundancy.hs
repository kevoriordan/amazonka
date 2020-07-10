{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.HasLogicalRedundancy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DirectConnect.Types.HasLogicalRedundancy (
  HasLogicalRedundancy (
    ..
    , HLRNO
    , HLRUnknown
    , HLRYes
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data HasLogicalRedundancy = HasLogicalRedundancy' (CI
                                                     Text)
                              deriving (Eq, Ord, Read, Show, Data, Typeable,
                                        Generic)

pattern HLRNO :: HasLogicalRedundancy
pattern HLRNO = HasLogicalRedundancy' "no"

pattern HLRUnknown :: HasLogicalRedundancy
pattern HLRUnknown = HasLogicalRedundancy' "unknown"

pattern HLRYes :: HasLogicalRedundancy
pattern HLRYes = HasLogicalRedundancy' "yes"

{-# COMPLETE
  HLRNO,
  HLRUnknown,
  HLRYes,
  HasLogicalRedundancy' #-}

instance FromText HasLogicalRedundancy where
    parser = (HasLogicalRedundancy' . mk) <$> takeText

instance ToText HasLogicalRedundancy where
    toText (HasLogicalRedundancy' ci) = original ci

-- | Represents an enum of /known/ $HasLogicalRedundancy.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum HasLogicalRedundancy where
    toEnum i = case i of
        0 -> HLRNO
        1 -> HLRUnknown
        2 -> HLRYes
        _ -> (error . showText) $ "Unknown index for HasLogicalRedundancy: " <> toText i
    fromEnum x = case x of
        HLRNO -> 0
        HLRUnknown -> 1
        HLRYes -> 2
        HasLogicalRedundancy' name -> (error . showText) $ "Unknown HasLogicalRedundancy: " <> original name

-- | Represents the bounds of /known/ $HasLogicalRedundancy.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded HasLogicalRedundancy where
    minBound = HLRNO
    maxBound = HLRYes

instance Hashable     HasLogicalRedundancy
instance NFData       HasLogicalRedundancy
instance ToByteString HasLogicalRedundancy
instance ToQuery      HasLogicalRedundancy
instance ToHeader     HasLogicalRedundancy

instance FromJSON HasLogicalRedundancy where
    parseJSON = parseJSONText "HasLogicalRedundancy"
