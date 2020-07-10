{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.DirectorySize
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DirectoryService.Types.DirectorySize (
  DirectorySize (
    ..
    , Large
    , Small
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data DirectorySize = DirectorySize' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern Large :: DirectorySize
pattern Large = DirectorySize' "Large"

pattern Small :: DirectorySize
pattern Small = DirectorySize' "Small"

{-# COMPLETE
  Large,
  Small,
  DirectorySize' #-}

instance FromText DirectorySize where
    parser = (DirectorySize' . mk) <$> takeText

instance ToText DirectorySize where
    toText (DirectorySize' ci) = original ci

-- | Represents an enum of /known/ $DirectorySize.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DirectorySize where
    toEnum i = case i of
        0 -> Large
        1 -> Small
        _ -> (error . showText) $ "Unknown index for DirectorySize: " <> toText i
    fromEnum x = case x of
        Large -> 0
        Small -> 1
        DirectorySize' name -> (error . showText) $ "Unknown DirectorySize: " <> original name

-- | Represents the bounds of /known/ $DirectorySize.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DirectorySize where
    minBound = Large
    maxBound = Small

instance Hashable     DirectorySize
instance NFData       DirectorySize
instance ToByteString DirectorySize
instance ToQuery      DirectorySize
instance ToHeader     DirectorySize

instance ToJSON DirectorySize where
    toJSON = toJSONText

instance FromJSON DirectorySize where
    parseJSON = parseJSONText "DirectorySize"
