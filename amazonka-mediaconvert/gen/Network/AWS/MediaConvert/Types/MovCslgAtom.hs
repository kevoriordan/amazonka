{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.MovCslgAtom
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.MovCslgAtom (
  MovCslgAtom (
    ..
    , MCAExclude
    , MCAInclude
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | When enabled, file composition times will start at zero, composition times in the 'ctts' (composition time to sample) box for B-frames will be negative, and a 'cslg' (composition shift least greatest) box will be included per 14496-1 amendment 1. This improves compatibility with Apple players and tools.
data MovCslgAtom = MovCslgAtom' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern MCAExclude :: MovCslgAtom
pattern MCAExclude = MovCslgAtom' "EXCLUDE"

pattern MCAInclude :: MovCslgAtom
pattern MCAInclude = MovCslgAtom' "INCLUDE"

{-# COMPLETE
  MCAExclude,
  MCAInclude,
  MovCslgAtom' #-}

instance FromText MovCslgAtom where
    parser = (MovCslgAtom' . mk) <$> takeText

instance ToText MovCslgAtom where
    toText (MovCslgAtom' ci) = original ci

-- | Represents an enum of /known/ $MovCslgAtom.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum MovCslgAtom where
    toEnum i = case i of
        0 -> MCAExclude
        1 -> MCAInclude
        _ -> (error . showText) $ "Unknown index for MovCslgAtom: " <> toText i
    fromEnum x = case x of
        MCAExclude -> 0
        MCAInclude -> 1
        MovCslgAtom' name -> (error . showText) $ "Unknown MovCslgAtom: " <> original name

-- | Represents the bounds of /known/ $MovCslgAtom.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded MovCslgAtom where
    minBound = MCAExclude
    maxBound = MCAInclude

instance Hashable     MovCslgAtom
instance NFData       MovCslgAtom
instance ToByteString MovCslgAtom
instance ToQuery      MovCslgAtom
instance ToHeader     MovCslgAtom

instance ToJSON MovCslgAtom where
    toJSON = toJSONText

instance FromJSON MovCslgAtom where
    parseJSON = parseJSONText "MovCslgAtom"
