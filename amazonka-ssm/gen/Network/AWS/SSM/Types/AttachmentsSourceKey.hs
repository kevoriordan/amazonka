{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.AttachmentsSourceKey
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.AttachmentsSourceKey (
  AttachmentsSourceKey (
    ..
    , AttachmentReference
    , S3FileURL
    , SourceURL
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AttachmentsSourceKey = AttachmentsSourceKey' (CI
                                                     Text)
                              deriving (Eq, Ord, Read, Show, Data, Typeable,
                                        Generic)

pattern AttachmentReference :: AttachmentsSourceKey
pattern AttachmentReference = AttachmentsSourceKey' "AttachmentReference"

pattern S3FileURL :: AttachmentsSourceKey
pattern S3FileURL = AttachmentsSourceKey' "S3FileUrl"

pattern SourceURL :: AttachmentsSourceKey
pattern SourceURL = AttachmentsSourceKey' "SourceUrl"

{-# COMPLETE
  AttachmentReference,
  S3FileURL,
  SourceURL,
  AttachmentsSourceKey' #-}

instance FromText AttachmentsSourceKey where
    parser = (AttachmentsSourceKey' . mk) <$> takeText

instance ToText AttachmentsSourceKey where
    toText (AttachmentsSourceKey' ci) = original ci

-- | Represents an enum of /known/ $AttachmentsSourceKey.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AttachmentsSourceKey where
    toEnum i = case i of
        0 -> AttachmentReference
        1 -> S3FileURL
        2 -> SourceURL
        _ -> (error . showText) $ "Unknown index for AttachmentsSourceKey: " <> toText i
    fromEnum x = case x of
        AttachmentReference -> 0
        S3FileURL -> 1
        SourceURL -> 2
        AttachmentsSourceKey' name -> (error . showText) $ "Unknown AttachmentsSourceKey: " <> original name

-- | Represents the bounds of /known/ $AttachmentsSourceKey.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AttachmentsSourceKey where
    minBound = AttachmentReference
    maxBound = SourceURL

instance Hashable     AttachmentsSourceKey
instance NFData       AttachmentsSourceKey
instance ToByteString AttachmentsSourceKey
instance ToQuery      AttachmentsSourceKey
instance ToHeader     AttachmentsSourceKey

instance ToJSON AttachmentsSourceKey where
    toJSON = toJSONText
