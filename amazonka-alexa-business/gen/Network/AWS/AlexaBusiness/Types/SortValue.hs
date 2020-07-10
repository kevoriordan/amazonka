{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.SortValue
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AlexaBusiness.Types.SortValue (
  SortValue (
    ..
    , Asc
    , Desc
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data SortValue = SortValue' (CI Text)
                   deriving (Eq, Ord, Read, Show, Data, Typeable,
                             Generic)

pattern Asc :: SortValue
pattern Asc = SortValue' "ASC"

pattern Desc :: SortValue
pattern Desc = SortValue' "DESC"

{-# COMPLETE
  Asc,
  Desc,
  SortValue' #-}

instance FromText SortValue where
    parser = (SortValue' . mk) <$> takeText

instance ToText SortValue where
    toText (SortValue' ci) = original ci

-- | Represents an enum of /known/ $SortValue.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum SortValue where
    toEnum i = case i of
        0 -> Asc
        1 -> Desc
        _ -> (error . showText) $ "Unknown index for SortValue: " <> toText i
    fromEnum x = case x of
        Asc -> 0
        Desc -> 1
        SortValue' name -> (error . showText) $ "Unknown SortValue: " <> original name

-- | Represents the bounds of /known/ $SortValue.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded SortValue where
    minBound = Asc
    maxBound = Desc

instance Hashable     SortValue
instance NFData       SortValue
instance ToByteString SortValue
instance ToQuery      SortValue
instance ToHeader     SortValue

instance ToJSON SortValue where
    toJSON = toJSONText
