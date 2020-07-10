{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaStoreData.Types.UploadAvailability
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaStoreData.Types.UploadAvailability (
  UploadAvailability (
    ..
    , Standard
    , Streaming
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data UploadAvailability = UploadAvailability' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern Standard :: UploadAvailability
pattern Standard = UploadAvailability' "STANDARD"

pattern Streaming :: UploadAvailability
pattern Streaming = UploadAvailability' "STREAMING"

{-# COMPLETE
  Standard,
  Streaming,
  UploadAvailability' #-}

instance FromText UploadAvailability where
    parser = (UploadAvailability' . mk) <$> takeText

instance ToText UploadAvailability where
    toText (UploadAvailability' ci) = original ci

-- | Represents an enum of /known/ $UploadAvailability.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum UploadAvailability where
    toEnum i = case i of
        0 -> Standard
        1 -> Streaming
        _ -> (error . showText) $ "Unknown index for UploadAvailability: " <> toText i
    fromEnum x = case x of
        Standard -> 0
        Streaming -> 1
        UploadAvailability' name -> (error . showText) $ "Unknown UploadAvailability: " <> original name

-- | Represents the bounds of /known/ $UploadAvailability.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded UploadAvailability where
    minBound = Standard
    maxBound = Streaming

instance Hashable     UploadAvailability
instance NFData       UploadAvailability
instance ToByteString UploadAvailability
instance ToQuery      UploadAvailability
instance ToHeader     UploadAvailability

instance ToJSON UploadAvailability where
    toJSON = toJSONText
