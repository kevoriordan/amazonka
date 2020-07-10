{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.SNSActionEncoding
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SES.Types.SNSActionEncoding (
  SNSActionEncoding (
    ..
    , BASE64
    , Utf8
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data SNSActionEncoding = SNSActionEncoding' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern BASE64 :: SNSActionEncoding
pattern BASE64 = SNSActionEncoding' "Base64"

pattern Utf8 :: SNSActionEncoding
pattern Utf8 = SNSActionEncoding' "UTF-8"

{-# COMPLETE
  BASE64,
  Utf8,
  SNSActionEncoding' #-}

instance FromText SNSActionEncoding where
    parser = (SNSActionEncoding' . mk) <$> takeText

instance ToText SNSActionEncoding where
    toText (SNSActionEncoding' ci) = original ci

-- | Represents an enum of /known/ $SNSActionEncoding.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum SNSActionEncoding where
    toEnum i = case i of
        0 -> BASE64
        1 -> Utf8
        _ -> (error . showText) $ "Unknown index for SNSActionEncoding: " <> toText i
    fromEnum x = case x of
        BASE64 -> 0
        Utf8 -> 1
        SNSActionEncoding' name -> (error . showText) $ "Unknown SNSActionEncoding: " <> original name

-- | Represents the bounds of /known/ $SNSActionEncoding.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded SNSActionEncoding where
    minBound = BASE64
    maxBound = Utf8

instance Hashable     SNSActionEncoding
instance NFData       SNSActionEncoding
instance ToByteString SNSActionEncoding
instance ToQuery      SNSActionEncoding
instance ToHeader     SNSActionEncoding

instance FromXML SNSActionEncoding where
    parseXML = parseXMLText "SNSActionEncoding"
