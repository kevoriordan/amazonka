{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.DirectoryEdition
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DirectoryService.Types.DirectoryEdition (
  DirectoryEdition (
    ..
    , Enterprise
    , Standard
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data DirectoryEdition = DirectoryEdition' (CI Text)
                          deriving (Eq, Ord, Read, Show, Data, Typeable,
                                    Generic)

pattern Enterprise :: DirectoryEdition
pattern Enterprise = DirectoryEdition' "Enterprise"

pattern Standard :: DirectoryEdition
pattern Standard = DirectoryEdition' "Standard"

{-# COMPLETE
  Enterprise,
  Standard,
  DirectoryEdition' #-}

instance FromText DirectoryEdition where
    parser = (DirectoryEdition' . mk) <$> takeText

instance ToText DirectoryEdition where
    toText (DirectoryEdition' ci) = original ci

-- | Represents an enum of /known/ $DirectoryEdition.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DirectoryEdition where
    toEnum i = case i of
        0 -> Enterprise
        1 -> Standard
        _ -> (error . showText) $ "Unknown index for DirectoryEdition: " <> toText i
    fromEnum x = case x of
        Enterprise -> 0
        Standard -> 1
        DirectoryEdition' name -> (error . showText) $ "Unknown DirectoryEdition: " <> original name

-- | Represents the bounds of /known/ $DirectoryEdition.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DirectoryEdition where
    minBound = Enterprise
    maxBound = Standard

instance Hashable     DirectoryEdition
instance NFData       DirectoryEdition
instance ToByteString DirectoryEdition
instance ToQuery      DirectoryEdition
instance ToHeader     DirectoryEdition

instance ToJSON DirectoryEdition where
    toJSON = toJSONText

instance FromJSON DirectoryEdition where
    parseJSON = parseJSONText "DirectoryEdition"
