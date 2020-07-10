{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.RetentionType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.RetentionType (
  RetentionType (
    ..
    , Delete
    , Retain
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data RetentionType = RetentionType' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern Delete :: RetentionType
pattern Delete = RetentionType' "Delete"

pattern Retain :: RetentionType
pattern Retain = RetentionType' "Retain"

{-# COMPLETE
  Delete,
  Retain,
  RetentionType' #-}

instance FromText RetentionType where
    parser = (RetentionType' . mk) <$> takeText

instance ToText RetentionType where
    toText (RetentionType' ci) = original ci

-- | Represents an enum of /known/ $RetentionType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum RetentionType where
    toEnum i = case i of
        0 -> Delete
        1 -> Retain
        _ -> (error . showText) $ "Unknown index for RetentionType: " <> toText i
    fromEnum x = case x of
        Delete -> 0
        Retain -> 1
        RetentionType' name -> (error . showText) $ "Unknown RetentionType: " <> original name

-- | Represents the bounds of /known/ $RetentionType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded RetentionType where
    minBound = Delete
    maxBound = Retain

instance Hashable     RetentionType
instance NFData       RetentionType
instance ToByteString RetentionType
instance ToQuery      RetentionType
instance ToHeader     RetentionType

instance ToJSON RetentionType where
    toJSON = toJSONText
