{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ServerSideEncryption
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.ServerSideEncryption (
  ServerSideEncryption (
    ..
    , AES256
    , AWSKMS
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude
import Network.AWS.S3.Internal

data ServerSideEncryption = ServerSideEncryption' (CI
                                                     Text)
                              deriving (Eq, Ord, Read, Show, Data, Typeable,
                                        Generic)

pattern AES256 :: ServerSideEncryption
pattern AES256 = ServerSideEncryption' "AES256"

pattern AWSKMS :: ServerSideEncryption
pattern AWSKMS = ServerSideEncryption' "aws:kms"

{-# COMPLETE
  AES256,
  AWSKMS,
  ServerSideEncryption' #-}

instance FromText ServerSideEncryption where
    parser = (ServerSideEncryption' . mk) <$> takeText

instance ToText ServerSideEncryption where
    toText (ServerSideEncryption' ci) = original ci

-- | Represents an enum of /known/ $ServerSideEncryption.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ServerSideEncryption where
    toEnum i = case i of
        0 -> AES256
        1 -> AWSKMS
        _ -> (error . showText) $ "Unknown index for ServerSideEncryption: " <> toText i
    fromEnum x = case x of
        AES256 -> 0
        AWSKMS -> 1
        ServerSideEncryption' name -> (error . showText) $ "Unknown ServerSideEncryption: " <> original name

-- | Represents the bounds of /known/ $ServerSideEncryption.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ServerSideEncryption where
    minBound = AES256
    maxBound = AWSKMS

instance Hashable     ServerSideEncryption
instance NFData       ServerSideEncryption
instance ToByteString ServerSideEncryption
instance ToQuery      ServerSideEncryption
instance ToHeader     ServerSideEncryption

instance FromXML ServerSideEncryption where
    parseXML = parseXMLText "ServerSideEncryption"

instance ToXML ServerSideEncryption where
    toXML = toXMLText
