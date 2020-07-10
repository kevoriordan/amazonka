{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.TypeDefinitionFormat
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AppSync.Types.TypeDefinitionFormat (
  TypeDefinitionFormat (
    ..
    , JSON
    , Sdl
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data TypeDefinitionFormat = TypeDefinitionFormat' (CI
                                                     Text)
                              deriving (Eq, Ord, Read, Show, Data, Typeable,
                                        Generic)

pattern JSON :: TypeDefinitionFormat
pattern JSON = TypeDefinitionFormat' "JSON"

pattern Sdl :: TypeDefinitionFormat
pattern Sdl = TypeDefinitionFormat' "SDL"

{-# COMPLETE
  JSON,
  Sdl,
  TypeDefinitionFormat' #-}

instance FromText TypeDefinitionFormat where
    parser = (TypeDefinitionFormat' . mk) <$> takeText

instance ToText TypeDefinitionFormat where
    toText (TypeDefinitionFormat' ci) = original ci

-- | Represents an enum of /known/ $TypeDefinitionFormat.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum TypeDefinitionFormat where
    toEnum i = case i of
        0 -> JSON
        1 -> Sdl
        _ -> (error . showText) $ "Unknown index for TypeDefinitionFormat: " <> toText i
    fromEnum x = case x of
        JSON -> 0
        Sdl -> 1
        TypeDefinitionFormat' name -> (error . showText) $ "Unknown TypeDefinitionFormat: " <> original name

-- | Represents the bounds of /known/ $TypeDefinitionFormat.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded TypeDefinitionFormat where
    minBound = JSON
    maxBound = Sdl

instance Hashable     TypeDefinitionFormat
instance NFData       TypeDefinitionFormat
instance ToByteString TypeDefinitionFormat
instance ToQuery      TypeDefinitionFormat
instance ToHeader     TypeDefinitionFormat

instance ToJSON TypeDefinitionFormat where
    toJSON = toJSONText

instance FromJSON TypeDefinitionFormat where
    parseJSON = parseJSONText "TypeDefinitionFormat"
