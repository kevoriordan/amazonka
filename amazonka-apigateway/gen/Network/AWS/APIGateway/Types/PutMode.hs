{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.PutMode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.APIGateway.Types.PutMode (
  PutMode (
    ..
    , Merge
    , Overwrite
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data PutMode = PutMode' (CI Text)
                 deriving (Eq, Ord, Read, Show, Data, Typeable,
                           Generic)

pattern Merge :: PutMode
pattern Merge = PutMode' "merge"

pattern Overwrite :: PutMode
pattern Overwrite = PutMode' "overwrite"

{-# COMPLETE
  Merge,
  Overwrite,
  PutMode' #-}

instance FromText PutMode where
    parser = (PutMode' . mk) <$> takeText

instance ToText PutMode where
    toText (PutMode' ci) = original ci

-- | Represents an enum of /known/ $PutMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum PutMode where
    toEnum i = case i of
        0 -> Merge
        1 -> Overwrite
        _ -> (error . showText) $ "Unknown index for PutMode: " <> toText i
    fromEnum x = case x of
        Merge -> 0
        Overwrite -> 1
        PutMode' name -> (error . showText) $ "Unknown PutMode: " <> original name

-- | Represents the bounds of /known/ $PutMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded PutMode where
    minBound = Merge
    maxBound = Overwrite

instance Hashable     PutMode
instance NFData       PutMode
instance ToByteString PutMode
instance ToQuery      PutMode
instance ToHeader     PutMode

instance ToJSON PutMode where
    toJSON = toJSONText
