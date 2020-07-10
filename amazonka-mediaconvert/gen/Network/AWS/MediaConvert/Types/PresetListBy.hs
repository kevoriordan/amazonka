{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.PresetListBy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.PresetListBy (
  PresetListBy (
    ..
    , PLBCreationDate
    , PLBName
    , PLBSystem
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Optional. When you request a list of presets, you can choose to list them alphabetically by NAME or chronologically by CREATION_DATE. If you don't specify, the service will list them by name.
data PresetListBy = PresetListBy' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern PLBCreationDate :: PresetListBy
pattern PLBCreationDate = PresetListBy' "CREATION_DATE"

pattern PLBName :: PresetListBy
pattern PLBName = PresetListBy' "NAME"

pattern PLBSystem :: PresetListBy
pattern PLBSystem = PresetListBy' "SYSTEM"

{-# COMPLETE
  PLBCreationDate,
  PLBName,
  PLBSystem,
  PresetListBy' #-}

instance FromText PresetListBy where
    parser = (PresetListBy' . mk) <$> takeText

instance ToText PresetListBy where
    toText (PresetListBy' ci) = original ci

-- | Represents an enum of /known/ $PresetListBy.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum PresetListBy where
    toEnum i = case i of
        0 -> PLBCreationDate
        1 -> PLBName
        2 -> PLBSystem
        _ -> (error . showText) $ "Unknown index for PresetListBy: " <> toText i
    fromEnum x = case x of
        PLBCreationDate -> 0
        PLBName -> 1
        PLBSystem -> 2
        PresetListBy' name -> (error . showText) $ "Unknown PresetListBy: " <> original name

-- | Represents the bounds of /known/ $PresetListBy.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded PresetListBy where
    minBound = PLBCreationDate
    maxBound = PLBSystem

instance Hashable     PresetListBy
instance NFData       PresetListBy
instance ToByteString PresetListBy
instance ToQuery      PresetListBy
instance ToHeader     PresetListBy

instance ToJSON PresetListBy where
    toJSON = toJSONText
