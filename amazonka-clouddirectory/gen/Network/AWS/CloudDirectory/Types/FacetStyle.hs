{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.FacetStyle
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudDirectory.Types.FacetStyle (
  FacetStyle (
    ..
    , Dynamic
    , Static
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data FacetStyle = FacetStyle' (CI Text)
                    deriving (Eq, Ord, Read, Show, Data, Typeable,
                              Generic)

pattern Dynamic :: FacetStyle
pattern Dynamic = FacetStyle' "DYNAMIC"

pattern Static :: FacetStyle
pattern Static = FacetStyle' "STATIC"

{-# COMPLETE
  Dynamic,
  Static,
  FacetStyle' #-}

instance FromText FacetStyle where
    parser = (FacetStyle' . mk) <$> takeText

instance ToText FacetStyle where
    toText (FacetStyle' ci) = original ci

-- | Represents an enum of /known/ $FacetStyle.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum FacetStyle where
    toEnum i = case i of
        0 -> Dynamic
        1 -> Static
        _ -> (error . showText) $ "Unknown index for FacetStyle: " <> toText i
    fromEnum x = case x of
        Dynamic -> 0
        Static -> 1
        FacetStyle' name -> (error . showText) $ "Unknown FacetStyle: " <> original name

-- | Represents the bounds of /known/ $FacetStyle.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded FacetStyle where
    minBound = Dynamic
    maxBound = Static

instance Hashable     FacetStyle
instance NFData       FacetStyle
instance ToByteString FacetStyle
instance ToQuery      FacetStyle
instance ToHeader     FacetStyle

instance ToJSON FacetStyle where
    toJSON = toJSONText

instance FromJSON FacetStyle where
    parseJSON = parseJSONText "FacetStyle"
