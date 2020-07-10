{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.RedirectActionStatusCodeEnum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ELBv2.Types.RedirectActionStatusCodeEnum (
  RedirectActionStatusCodeEnum (
    ..
    , HTTP301
    , HTTP302
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data RedirectActionStatusCodeEnum = RedirectActionStatusCodeEnum' (CI
                                                                     Text)
                                      deriving (Eq, Ord, Read, Show, Data,
                                                Typeable, Generic)

pattern HTTP301 :: RedirectActionStatusCodeEnum
pattern HTTP301 = RedirectActionStatusCodeEnum' "HTTP_301"

pattern HTTP302 :: RedirectActionStatusCodeEnum
pattern HTTP302 = RedirectActionStatusCodeEnum' "HTTP_302"

{-# COMPLETE
  HTTP301,
  HTTP302,
  RedirectActionStatusCodeEnum' #-}

instance FromText RedirectActionStatusCodeEnum where
    parser = (RedirectActionStatusCodeEnum' . mk) <$> takeText

instance ToText RedirectActionStatusCodeEnum where
    toText (RedirectActionStatusCodeEnum' ci) = original ci

-- | Represents an enum of /known/ $RedirectActionStatusCodeEnum.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum RedirectActionStatusCodeEnum where
    toEnum i = case i of
        0 -> HTTP301
        1 -> HTTP302
        _ -> (error . showText) $ "Unknown index for RedirectActionStatusCodeEnum: " <> toText i
    fromEnum x = case x of
        HTTP301 -> 0
        HTTP302 -> 1
        RedirectActionStatusCodeEnum' name -> (error . showText) $ "Unknown RedirectActionStatusCodeEnum: " <> original name

-- | Represents the bounds of /known/ $RedirectActionStatusCodeEnum.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded RedirectActionStatusCodeEnum where
    minBound = HTTP301
    maxBound = HTTP302

instance Hashable     RedirectActionStatusCodeEnum
instance NFData       RedirectActionStatusCodeEnum
instance ToByteString RedirectActionStatusCodeEnum
instance ToQuery      RedirectActionStatusCodeEnum
instance ToHeader     RedirectActionStatusCodeEnum

instance FromXML RedirectActionStatusCodeEnum where
    parseXML = parseXMLText "RedirectActionStatusCodeEnum"
