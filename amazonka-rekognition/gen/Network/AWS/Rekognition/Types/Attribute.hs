{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.Attribute
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Rekognition.Types.Attribute (
  Attribute (
    ..
    , All
    , Default
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data Attribute = Attribute' (CI Text)
                   deriving (Eq, Ord, Read, Show, Data, Typeable,
                             Generic)

pattern All :: Attribute
pattern All = Attribute' "ALL"

pattern Default :: Attribute
pattern Default = Attribute' "DEFAULT"

{-# COMPLETE
  All,
  Default,
  Attribute' #-}

instance FromText Attribute where
    parser = (Attribute' . mk) <$> takeText

instance ToText Attribute where
    toText (Attribute' ci) = original ci

-- | Represents an enum of /known/ $Attribute.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Attribute where
    toEnum i = case i of
        0 -> All
        1 -> Default
        _ -> (error . showText) $ "Unknown index for Attribute: " <> toText i
    fromEnum x = case x of
        All -> 0
        Default -> 1
        Attribute' name -> (error . showText) $ "Unknown Attribute: " <> original name

-- | Represents the bounds of /known/ $Attribute.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Attribute where
    minBound = All
    maxBound = Default

instance Hashable     Attribute
instance NFData       Attribute
instance ToByteString Attribute
instance ToQuery      Attribute
instance ToHeader     Attribute

instance ToJSON Attribute where
    toJSON = toJSONText
