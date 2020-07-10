{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.AttachmentHashType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.AttachmentHashType (
  AttachmentHashType (
    ..
    , AttHashSHA256
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AttachmentHashType = AttachmentHashType' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern AttHashSHA256 :: AttachmentHashType
pattern AttHashSHA256 = AttachmentHashType' "Sha256"

{-# COMPLETE
  AttHashSHA256,
  AttachmentHashType' #-}

instance FromText AttachmentHashType where
    parser = (AttachmentHashType' . mk) <$> takeText

instance ToText AttachmentHashType where
    toText (AttachmentHashType' ci) = original ci

-- | Represents an enum of /known/ $AttachmentHashType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AttachmentHashType where
    toEnum i = case i of
        0 -> AttHashSHA256
        _ -> (error . showText) $ "Unknown index for AttachmentHashType: " <> toText i
    fromEnum x = case x of
        AttHashSHA256 -> 0
        AttachmentHashType' name -> (error . showText) $ "Unknown AttachmentHashType: " <> original name

-- | Represents the bounds of /known/ $AttachmentHashType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AttachmentHashType where
    minBound = AttHashSHA256
    maxBound = AttHashSHA256

instance Hashable     AttachmentHashType
instance NFData       AttachmentHashType
instance ToByteString AttachmentHashType
instance ToQuery      AttachmentHashType
instance ToHeader     AttachmentHashType

instance FromJSON AttachmentHashType where
    parseJSON = parseJSONText "AttachmentHashType"
