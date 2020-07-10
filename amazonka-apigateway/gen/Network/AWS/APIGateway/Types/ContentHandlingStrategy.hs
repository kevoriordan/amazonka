{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.ContentHandlingStrategy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.APIGateway.Types.ContentHandlingStrategy (
  ContentHandlingStrategy (
    ..
    , ConvertToBinary
    , ConvertToText
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ContentHandlingStrategy = ContentHandlingStrategy' (CI
                                                           Text)
                                 deriving (Eq, Ord, Read, Show, Data, Typeable,
                                           Generic)

pattern ConvertToBinary :: ContentHandlingStrategy
pattern ConvertToBinary = ContentHandlingStrategy' "CONVERT_TO_BINARY"

pattern ConvertToText :: ContentHandlingStrategy
pattern ConvertToText = ContentHandlingStrategy' "CONVERT_TO_TEXT"

{-# COMPLETE
  ConvertToBinary,
  ConvertToText,
  ContentHandlingStrategy' #-}

instance FromText ContentHandlingStrategy where
    parser = (ContentHandlingStrategy' . mk) <$> takeText

instance ToText ContentHandlingStrategy where
    toText (ContentHandlingStrategy' ci) = original ci

-- | Represents an enum of /known/ $ContentHandlingStrategy.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ContentHandlingStrategy where
    toEnum i = case i of
        0 -> ConvertToBinary
        1 -> ConvertToText
        _ -> (error . showText) $ "Unknown index for ContentHandlingStrategy: " <> toText i
    fromEnum x = case x of
        ConvertToBinary -> 0
        ConvertToText -> 1
        ContentHandlingStrategy' name -> (error . showText) $ "Unknown ContentHandlingStrategy: " <> original name

-- | Represents the bounds of /known/ $ContentHandlingStrategy.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ContentHandlingStrategy where
    minBound = ConvertToBinary
    maxBound = ConvertToText

instance Hashable     ContentHandlingStrategy
instance NFData       ContentHandlingStrategy
instance ToByteString ContentHandlingStrategy
instance ToQuery      ContentHandlingStrategy
instance ToHeader     ContentHandlingStrategy

instance ToJSON ContentHandlingStrategy where
    toJSON = toJSONText

instance FromJSON ContentHandlingStrategy where
    parseJSON = parseJSONText "ContentHandlingStrategy"
