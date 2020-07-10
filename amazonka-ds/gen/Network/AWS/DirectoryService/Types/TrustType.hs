{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.TrustType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DirectoryService.Types.TrustType (
  TrustType (
    ..
    , External
    , Forest
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data TrustType = TrustType' (CI Text)
                   deriving (Eq, Ord, Read, Show, Data, Typeable,
                             Generic)

pattern External :: TrustType
pattern External = TrustType' "External"

pattern Forest :: TrustType
pattern Forest = TrustType' "Forest"

{-# COMPLETE
  External,
  Forest,
  TrustType' #-}

instance FromText TrustType where
    parser = (TrustType' . mk) <$> takeText

instance ToText TrustType where
    toText (TrustType' ci) = original ci

-- | Represents an enum of /known/ $TrustType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum TrustType where
    toEnum i = case i of
        0 -> External
        1 -> Forest
        _ -> (error . showText) $ "Unknown index for TrustType: " <> toText i
    fromEnum x = case x of
        External -> 0
        Forest -> 1
        TrustType' name -> (error . showText) $ "Unknown TrustType: " <> original name

-- | Represents the bounds of /known/ $TrustType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded TrustType where
    minBound = External
    maxBound = Forest

instance Hashable     TrustType
instance NFData       TrustType
instance ToByteString TrustType
instance ToQuery      TrustType
instance ToHeader     TrustType

instance ToJSON TrustType where
    toJSON = toJSONText

instance FromJSON TrustType where
    parseJSON = parseJSONText "TrustType"
