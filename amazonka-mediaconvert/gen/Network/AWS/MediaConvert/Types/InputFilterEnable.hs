{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.InputFilterEnable
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.InputFilterEnable (
  InputFilterEnable (
    ..
    , IFEAuto
    , IFEDisable
    , IFEForce
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Use Filter enable (InputFilterEnable) to specify how the transcoding service applies the denoise and deblock filters. You must also enable the filters separately, with Denoise (InputDenoiseFilter) and Deblock (InputDeblockFilter). * Auto - The transcoding service determines whether to apply filtering, depending on input type and quality. * Disable - The input is not filtered. This is true even if you use the API to enable them in (InputDeblockFilter) and (InputDeblockFilter). * Force - The in put is filtered regardless of input type.
data InputFilterEnable = InputFilterEnable' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern IFEAuto :: InputFilterEnable
pattern IFEAuto = InputFilterEnable' "AUTO"

pattern IFEDisable :: InputFilterEnable
pattern IFEDisable = InputFilterEnable' "DISABLE"

pattern IFEForce :: InputFilterEnable
pattern IFEForce = InputFilterEnable' "FORCE"

{-# COMPLETE
  IFEAuto,
  IFEDisable,
  IFEForce,
  InputFilterEnable' #-}

instance FromText InputFilterEnable where
    parser = (InputFilterEnable' . mk) <$> takeText

instance ToText InputFilterEnable where
    toText (InputFilterEnable' ci) = original ci

-- | Represents an enum of /known/ $InputFilterEnable.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum InputFilterEnable where
    toEnum i = case i of
        0 -> IFEAuto
        1 -> IFEDisable
        2 -> IFEForce
        _ -> (error . showText) $ "Unknown index for InputFilterEnable: " <> toText i
    fromEnum x = case x of
        IFEAuto -> 0
        IFEDisable -> 1
        IFEForce -> 2
        InputFilterEnable' name -> (error . showText) $ "Unknown InputFilterEnable: " <> original name

-- | Represents the bounds of /known/ $InputFilterEnable.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded InputFilterEnable where
    minBound = IFEAuto
    maxBound = IFEForce

instance Hashable     InputFilterEnable
instance NFData       InputFilterEnable
instance ToByteString InputFilterEnable
instance ToQuery      InputFilterEnable
instance ToHeader     InputFilterEnable

instance ToJSON InputFilterEnable where
    toJSON = toJSONText

instance FromJSON InputFilterEnable where
    parseJSON = parseJSONText "InputFilterEnable"
