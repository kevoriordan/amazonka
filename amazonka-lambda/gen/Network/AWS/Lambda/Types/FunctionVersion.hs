{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.FunctionVersion
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lambda.Types.FunctionVersion (
  FunctionVersion (
    ..
    , All
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data FunctionVersion = FunctionVersion' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern All :: FunctionVersion
pattern All = FunctionVersion' "ALL"

{-# COMPLETE
  All,
  FunctionVersion' #-}

instance FromText FunctionVersion where
    parser = (FunctionVersion' . mk) <$> takeText

instance ToText FunctionVersion where
    toText (FunctionVersion' ci) = original ci

-- | Represents an enum of /known/ $FunctionVersion.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum FunctionVersion where
    toEnum i = case i of
        0 -> All
        _ -> (error . showText) $ "Unknown index for FunctionVersion: " <> toText i
    fromEnum x = case x of
        All -> 0
        FunctionVersion' name -> (error . showText) $ "Unknown FunctionVersion: " <> original name

-- | Represents the bounds of /known/ $FunctionVersion.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded FunctionVersion where
    minBound = All
    maxBound = All

instance Hashable     FunctionVersion
instance NFData       FunctionVersion
instance ToByteString FunctionVersion
instance ToQuery      FunctionVersion
instance ToHeader     FunctionVersion

instance ToJSON FunctionVersion where
    toJSON = toJSONText
