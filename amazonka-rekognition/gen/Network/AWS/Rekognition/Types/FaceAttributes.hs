{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.FaceAttributes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Rekognition.Types.FaceAttributes (
  FaceAttributes (
    ..
    , FAAll
    , FADefault
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data FaceAttributes = FaceAttributes' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern FAAll :: FaceAttributes
pattern FAAll = FaceAttributes' "ALL"

pattern FADefault :: FaceAttributes
pattern FADefault = FaceAttributes' "DEFAULT"

{-# COMPLETE
  FAAll,
  FADefault,
  FaceAttributes' #-}

instance FromText FaceAttributes where
    parser = (FaceAttributes' . mk) <$> takeText

instance ToText FaceAttributes where
    toText (FaceAttributes' ci) = original ci

-- | Represents an enum of /known/ $FaceAttributes.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum FaceAttributes where
    toEnum i = case i of
        0 -> FAAll
        1 -> FADefault
        _ -> (error . showText) $ "Unknown index for FaceAttributes: " <> toText i
    fromEnum x = case x of
        FAAll -> 0
        FADefault -> 1
        FaceAttributes' name -> (error . showText) $ "Unknown FaceAttributes: " <> original name

-- | Represents the bounds of /known/ $FaceAttributes.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded FaceAttributes where
    minBound = FAAll
    maxBound = FADefault

instance Hashable     FaceAttributes
instance NFData       FaceAttributes
instance ToByteString FaceAttributes
instance ToQuery      FaceAttributes
instance ToHeader     FaceAttributes

instance ToJSON FaceAttributes where
    toJSON = toJSONText
