{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.DirectoryType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DirectoryService.Types.DirectoryType (
  DirectoryType (
    ..
    , ADConnector
    , MicrosoftAD
    , SharedMicrosoftAD
    , SimpleAD
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data DirectoryType = DirectoryType' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern ADConnector :: DirectoryType
pattern ADConnector = DirectoryType' "ADConnector"

pattern MicrosoftAD :: DirectoryType
pattern MicrosoftAD = DirectoryType' "MicrosoftAD"

pattern SharedMicrosoftAD :: DirectoryType
pattern SharedMicrosoftAD = DirectoryType' "SharedMicrosoftAD"

pattern SimpleAD :: DirectoryType
pattern SimpleAD = DirectoryType' "SimpleAD"

{-# COMPLETE
  ADConnector,
  MicrosoftAD,
  SharedMicrosoftAD,
  SimpleAD,
  DirectoryType' #-}

instance FromText DirectoryType where
    parser = (DirectoryType' . mk) <$> takeText

instance ToText DirectoryType where
    toText (DirectoryType' ci) = original ci

-- | Represents an enum of /known/ $DirectoryType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DirectoryType where
    toEnum i = case i of
        0 -> ADConnector
        1 -> MicrosoftAD
        2 -> SharedMicrosoftAD
        3 -> SimpleAD
        _ -> (error . showText) $ "Unknown index for DirectoryType: " <> toText i
    fromEnum x = case x of
        ADConnector -> 0
        MicrosoftAD -> 1
        SharedMicrosoftAD -> 2
        SimpleAD -> 3
        DirectoryType' name -> (error . showText) $ "Unknown DirectoryType: " <> original name

-- | Represents the bounds of /known/ $DirectoryType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DirectoryType where
    minBound = ADConnector
    maxBound = SimpleAD

instance Hashable     DirectoryType
instance NFData       DirectoryType
instance ToByteString DirectoryType
instance ToQuery      DirectoryType
instance ToHeader     DirectoryType

instance FromJSON DirectoryType where
    parseJSON = parseJSONText "DirectoryType"
