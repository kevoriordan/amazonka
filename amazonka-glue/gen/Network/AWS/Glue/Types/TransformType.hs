{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.TransformType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.TransformType (
  TransformType (
    ..
    , FindMatches
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data TransformType = TransformType' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern FindMatches :: TransformType
pattern FindMatches = TransformType' "FIND_MATCHES"

{-# COMPLETE
  FindMatches,
  TransformType' #-}

instance FromText TransformType where
    parser = (TransformType' . mk) <$> takeText

instance ToText TransformType where
    toText (TransformType' ci) = original ci

-- | Represents an enum of /known/ $TransformType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum TransformType where
    toEnum i = case i of
        0 -> FindMatches
        _ -> (error . showText) $ "Unknown index for TransformType: " <> toText i
    fromEnum x = case x of
        FindMatches -> 0
        TransformType' name -> (error . showText) $ "Unknown TransformType: " <> original name

-- | Represents the bounds of /known/ $TransformType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded TransformType where
    minBound = FindMatches
    maxBound = FindMatches

instance Hashable     TransformType
instance NFData       TransformType
instance ToByteString TransformType
instance ToQuery      TransformType
instance ToHeader     TransformType

instance ToJSON TransformType where
    toJSON = toJSONText

instance FromJSON TransformType where
    parseJSON = parseJSONText "TransformType"
