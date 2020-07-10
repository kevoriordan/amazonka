{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.ObfuscationSetting
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.LexModels.Types.ObfuscationSetting (
  ObfuscationSetting (
    ..
    , DefaultObfuscation
    , None
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ObfuscationSetting = ObfuscationSetting' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern DefaultObfuscation :: ObfuscationSetting
pattern DefaultObfuscation = ObfuscationSetting' "DEFAULT_OBFUSCATION"

pattern None :: ObfuscationSetting
pattern None = ObfuscationSetting' "NONE"

{-# COMPLETE
  DefaultObfuscation,
  None,
  ObfuscationSetting' #-}

instance FromText ObfuscationSetting where
    parser = (ObfuscationSetting' . mk) <$> takeText

instance ToText ObfuscationSetting where
    toText (ObfuscationSetting' ci) = original ci

-- | Represents an enum of /known/ $ObfuscationSetting.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ObfuscationSetting where
    toEnum i = case i of
        0 -> DefaultObfuscation
        1 -> None
        _ -> (error . showText) $ "Unknown index for ObfuscationSetting: " <> toText i
    fromEnum x = case x of
        DefaultObfuscation -> 0
        None -> 1
        ObfuscationSetting' name -> (error . showText) $ "Unknown ObfuscationSetting: " <> original name

-- | Represents the bounds of /known/ $ObfuscationSetting.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ObfuscationSetting where
    minBound = DefaultObfuscation
    maxBound = None

instance Hashable     ObfuscationSetting
instance NFData       ObfuscationSetting
instance ToByteString ObfuscationSetting
instance ToQuery      ObfuscationSetting
instance ToHeader     ObfuscationSetting

instance ToJSON ObfuscationSetting where
    toJSON = toJSONText

instance FromJSON ObfuscationSetting where
    parseJSON = parseJSONText "ObfuscationSetting"
