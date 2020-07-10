{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Mp4CslgAtom
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.Mp4CslgAtom (
  Mp4CslgAtom (
    ..
    , Mp4Exclude
    , Mp4Include
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | When enabled, file composition times will start at zero, composition times in the 'ctts' (composition time to sample) box for B-frames will be negative, and a 'cslg' (composition shift least greatest) box will be included per 14496-1 amendment 1. This improves compatibility with Apple players and tools.
data Mp4CslgAtom = Mp4CslgAtom' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern Mp4Exclude :: Mp4CslgAtom
pattern Mp4Exclude = Mp4CslgAtom' "EXCLUDE"

pattern Mp4Include :: Mp4CslgAtom
pattern Mp4Include = Mp4CslgAtom' "INCLUDE"

{-# COMPLETE
  Mp4Exclude,
  Mp4Include,
  Mp4CslgAtom' #-}

instance FromText Mp4CslgAtom where
    parser = (Mp4CslgAtom' . mk) <$> takeText

instance ToText Mp4CslgAtom where
    toText (Mp4CslgAtom' ci) = original ci

-- | Represents an enum of /known/ $Mp4CslgAtom.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Mp4CslgAtom where
    toEnum i = case i of
        0 -> Mp4Exclude
        1 -> Mp4Include
        _ -> (error . showText) $ "Unknown index for Mp4CslgAtom: " <> toText i
    fromEnum x = case x of
        Mp4Exclude -> 0
        Mp4Include -> 1
        Mp4CslgAtom' name -> (error . showText) $ "Unknown Mp4CslgAtom: " <> original name

-- | Represents the bounds of /known/ $Mp4CslgAtom.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Mp4CslgAtom where
    minBound = Mp4Exclude
    maxBound = Mp4Include

instance Hashable     Mp4CslgAtom
instance NFData       Mp4CslgAtom
instance ToByteString Mp4CslgAtom
instance ToQuery      Mp4CslgAtom
instance ToHeader     Mp4CslgAtom

instance ToJSON Mp4CslgAtom where
    toJSON = toJSONText

instance FromJSON Mp4CslgAtom where
    parseJSON = parseJSONText "Mp4CslgAtom"
