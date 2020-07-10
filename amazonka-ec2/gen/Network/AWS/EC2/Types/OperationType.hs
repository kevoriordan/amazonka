{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.OperationType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.OperationType (
  OperationType (
    ..
    , Add
    , Remove
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data OperationType = OperationType' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern Add :: OperationType
pattern Add = OperationType' "add"

pattern Remove :: OperationType
pattern Remove = OperationType' "remove"

{-# COMPLETE
  Add,
  Remove,
  OperationType' #-}

instance FromText OperationType where
    parser = (OperationType' . mk) <$> takeText

instance ToText OperationType where
    toText (OperationType' ci) = original ci

-- | Represents an enum of /known/ $OperationType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum OperationType where
    toEnum i = case i of
        0 -> Add
        1 -> Remove
        _ -> (error . showText) $ "Unknown index for OperationType: " <> toText i
    fromEnum x = case x of
        Add -> 0
        Remove -> 1
        OperationType' name -> (error . showText) $ "Unknown OperationType: " <> original name

-- | Represents the bounds of /known/ $OperationType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded OperationType where
    minBound = Add
    maxBound = Remove

instance Hashable     OperationType
instance NFData       OperationType
instance ToByteString OperationType
instance ToQuery      OperationType
instance ToHeader     OperationType
