{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.Include
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.Include (
  Include (
    ..
    , All
    , Any
    , None
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data Include = Include' (CI Text)
                 deriving (Eq, Ord, Read, Show, Data, Typeable,
                           Generic)

pattern All :: Include
pattern All = Include' "ALL"

pattern Any :: Include
pattern Any = Include' "ANY"

pattern None :: Include
pattern None = Include' "NONE"

{-# COMPLETE
  All,
  Any,
  None,
  Include' #-}

instance FromText Include where
    parser = (Include' . mk) <$> takeText

instance ToText Include where
    toText (Include' ci) = original ci

-- | Represents an enum of /known/ $Include.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Include where
    toEnum i = case i of
        0 -> All
        1 -> Any
        2 -> None
        _ -> (error . showText) $ "Unknown index for Include: " <> toText i
    fromEnum x = case x of
        All -> 0
        Any -> 1
        None -> 2
        Include' name -> (error . showText) $ "Unknown Include: " <> original name

-- | Represents the bounds of /known/ $Include.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Include where
    minBound = All
    maxBound = None

instance Hashable     Include
instance NFData       Include
instance ToByteString Include
instance ToQuery      Include
instance ToHeader     Include

instance ToJSON Include where
    toJSON = toJSONText

instance FromJSON Include where
    parseJSON = parseJSONText "Include"
