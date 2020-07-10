{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.Sort
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.Sort (
  Sort (
    ..
    , Asc
    , Desc
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data Sort = Sort' (CI Text)
              deriving (Eq, Ord, Read, Show, Data, Typeable,
                        Generic)

pattern Asc :: Sort
pattern Asc = Sort' "ASC"

pattern Desc :: Sort
pattern Desc = Sort' "DESC"

{-# COMPLETE
  Asc,
  Desc,
  Sort' #-}

instance FromText Sort where
    parser = (Sort' . mk) <$> takeText

instance ToText Sort where
    toText (Sort' ci) = original ci

-- | Represents an enum of /known/ $Sort.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Sort where
    toEnum i = case i of
        0 -> Asc
        1 -> Desc
        _ -> (error . showText) $ "Unknown index for Sort: " <> toText i
    fromEnum x = case x of
        Asc -> 0
        Desc -> 1
        Sort' name -> (error . showText) $ "Unknown Sort: " <> original name

-- | Represents the bounds of /known/ $Sort.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Sort where
    minBound = Asc
    maxBound = Desc

instance Hashable     Sort
instance NFData       Sort
instance ToByteString Sort
instance ToQuery      Sort
instance ToHeader     Sort

instance ToJSON Sort where
    toJSON = toJSONText
