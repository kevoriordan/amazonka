{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.StackSetOperationType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ServiceCatalog.Types.StackSetOperationType (
  StackSetOperationType (
    ..
    , Create
    , Delete
    , Update
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data StackSetOperationType = StackSetOperationType' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern Create :: StackSetOperationType
pattern Create = StackSetOperationType' "CREATE"

pattern Delete :: StackSetOperationType
pattern Delete = StackSetOperationType' "DELETE"

pattern Update :: StackSetOperationType
pattern Update = StackSetOperationType' "UPDATE"

{-# COMPLETE
  Create,
  Delete,
  Update,
  StackSetOperationType' #-}

instance FromText StackSetOperationType where
    parser = (StackSetOperationType' . mk) <$> takeText

instance ToText StackSetOperationType where
    toText (StackSetOperationType' ci) = original ci

-- | Represents an enum of /known/ $StackSetOperationType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum StackSetOperationType where
    toEnum i = case i of
        0 -> Create
        1 -> Delete
        2 -> Update
        _ -> (error . showText) $ "Unknown index for StackSetOperationType: " <> toText i
    fromEnum x = case x of
        Create -> 0
        Delete -> 1
        Update -> 2
        StackSetOperationType' name -> (error . showText) $ "Unknown StackSetOperationType: " <> original name

-- | Represents the bounds of /known/ $StackSetOperationType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded StackSetOperationType where
    minBound = Create
    maxBound = Update

instance Hashable     StackSetOperationType
instance NFData       StackSetOperationType
instance ToByteString StackSetOperationType
instance ToQuery      StackSetOperationType
instance ToHeader     StackSetOperationType

instance ToJSON StackSetOperationType where
    toJSON = toJSONText
