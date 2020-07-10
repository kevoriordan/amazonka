{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.Logical
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.Logical (
  Logical (
    ..
    , And
    , Any
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data Logical = Logical' (CI Text)
                 deriving (Eq, Ord, Read, Show, Data, Typeable,
                           Generic)

pattern And :: Logical
pattern And = Logical' "AND"

pattern Any :: Logical
pattern Any = Logical' "ANY"

{-# COMPLETE
  And,
  Any,
  Logical' #-}

instance FromText Logical where
    parser = (Logical' . mk) <$> takeText

instance ToText Logical where
    toText (Logical' ci) = original ci

-- | Represents an enum of /known/ $Logical.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Logical where
    toEnum i = case i of
        0 -> And
        1 -> Any
        _ -> (error . showText) $ "Unknown index for Logical: " <> toText i
    fromEnum x = case x of
        And -> 0
        Any -> 1
        Logical' name -> (error . showText) $ "Unknown Logical: " <> original name

-- | Represents the bounds of /known/ $Logical.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Logical where
    minBound = And
    maxBound = Any

instance Hashable     Logical
instance NFData       Logical
instance ToByteString Logical
instance ToQuery      Logical
instance ToHeader     Logical

instance ToJSON Logical where
    toJSON = toJSONText

instance FromJSON Logical where
    parseJSON = parseJSONText "Logical"
