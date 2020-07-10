{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.APIKeysFormat
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.APIGateway.Types.APIKeysFormat (
  APIKeysFormat (
    ..
    , CSV
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data APIKeysFormat = APIKeysFormat' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern CSV :: APIKeysFormat
pattern CSV = APIKeysFormat' "csv"

{-# COMPLETE
  CSV,
  APIKeysFormat' #-}

instance FromText APIKeysFormat where
    parser = (APIKeysFormat' . mk) <$> takeText

instance ToText APIKeysFormat where
    toText (APIKeysFormat' ci) = original ci

-- | Represents an enum of /known/ $APIKeysFormat.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum APIKeysFormat where
    toEnum i = case i of
        0 -> CSV
        _ -> (error . showText) $ "Unknown index for APIKeysFormat: " <> toText i
    fromEnum x = case x of
        CSV -> 0
        APIKeysFormat' name -> (error . showText) $ "Unknown APIKeysFormat: " <> original name

-- | Represents the bounds of /known/ $APIKeysFormat.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded APIKeysFormat where
    minBound = CSV
    maxBound = CSV

instance Hashable     APIKeysFormat
instance NFData       APIKeysFormat
instance ToByteString APIKeysFormat
instance ToQuery      APIKeysFormat
instance ToHeader     APIKeysFormat

instance ToJSON APIKeysFormat where
    toJSON = toJSONText
