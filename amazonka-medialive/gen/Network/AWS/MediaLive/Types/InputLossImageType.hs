{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputLossImageType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.InputLossImageType (
  InputLossImageType (
    ..
    , Color
    , Slate
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Input Loss Image Type
data InputLossImageType = InputLossImageType' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern Color :: InputLossImageType
pattern Color = InputLossImageType' "COLOR"

pattern Slate :: InputLossImageType
pattern Slate = InputLossImageType' "SLATE"

{-# COMPLETE
  Color,
  Slate,
  InputLossImageType' #-}

instance FromText InputLossImageType where
    parser = (InputLossImageType' . mk) <$> takeText

instance ToText InputLossImageType where
    toText (InputLossImageType' ci) = original ci

-- | Represents an enum of /known/ $InputLossImageType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum InputLossImageType where
    toEnum i = case i of
        0 -> Color
        1 -> Slate
        _ -> (error . showText) $ "Unknown index for InputLossImageType: " <> toText i
    fromEnum x = case x of
        Color -> 0
        Slate -> 1
        InputLossImageType' name -> (error . showText) $ "Unknown InputLossImageType: " <> original name

-- | Represents the bounds of /known/ $InputLossImageType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded InputLossImageType where
    minBound = Color
    maxBound = Slate

instance Hashable     InputLossImageType
instance NFData       InputLossImageType
instance ToByteString InputLossImageType
instance ToQuery      InputLossImageType
instance ToHeader     InputLossImageType

instance ToJSON InputLossImageType where
    toJSON = toJSONText

instance FromJSON InputLossImageType where
    parseJSON = parseJSONText "InputLossImageType"
