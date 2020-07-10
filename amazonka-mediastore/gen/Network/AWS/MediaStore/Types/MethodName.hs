{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaStore.Types.MethodName
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaStore.Types.MethodName (
  MethodName (
    ..
    , Delete
    , Get
    , Head
    , Put
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data MethodName = MethodName' (CI Text)
                    deriving (Eq, Ord, Read, Show, Data, Typeable,
                              Generic)

pattern Delete :: MethodName
pattern Delete = MethodName' "DELETE"

pattern Get :: MethodName
pattern Get = MethodName' "GET"

pattern Head :: MethodName
pattern Head = MethodName' "HEAD"

pattern Put :: MethodName
pattern Put = MethodName' "PUT"

{-# COMPLETE
  Delete,
  Get,
  Head,
  Put,
  MethodName' #-}

instance FromText MethodName where
    parser = (MethodName' . mk) <$> takeText

instance ToText MethodName where
    toText (MethodName' ci) = original ci

-- | Represents an enum of /known/ $MethodName.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum MethodName where
    toEnum i = case i of
        0 -> Delete
        1 -> Get
        2 -> Head
        3 -> Put
        _ -> (error . showText) $ "Unknown index for MethodName: " <> toText i
    fromEnum x = case x of
        Delete -> 0
        Get -> 1
        Head -> 2
        Put -> 3
        MethodName' name -> (error . showText) $ "Unknown MethodName: " <> original name

-- | Represents the bounds of /known/ $MethodName.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded MethodName where
    minBound = Delete
    maxBound = Put

instance Hashable     MethodName
instance NFData       MethodName
instance ToByteString MethodName
instance ToQuery      MethodName
instance ToHeader     MethodName

instance ToJSON MethodName where
    toJSON = toJSONText

instance FromJSON MethodName where
    parseJSON = parseJSONText "MethodName"
