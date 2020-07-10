{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Eac3AtmosMeteringMode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.Eac3AtmosMeteringMode (
  Eac3AtmosMeteringMode (
    ..
    , EAMMItuBs17701
    , EAMMItuBs17702
    , EAMMItuBs17703
    , EAMMItuBs17704
    , EAMMLeqA
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Choose how the service meters the loudness of your audio.
data Eac3AtmosMeteringMode = Eac3AtmosMeteringMode' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern EAMMItuBs17701 :: Eac3AtmosMeteringMode
pattern EAMMItuBs17701 = Eac3AtmosMeteringMode' "ITU_BS_1770_1"

pattern EAMMItuBs17702 :: Eac3AtmosMeteringMode
pattern EAMMItuBs17702 = Eac3AtmosMeteringMode' "ITU_BS_1770_2"

pattern EAMMItuBs17703 :: Eac3AtmosMeteringMode
pattern EAMMItuBs17703 = Eac3AtmosMeteringMode' "ITU_BS_1770_3"

pattern EAMMItuBs17704 :: Eac3AtmosMeteringMode
pattern EAMMItuBs17704 = Eac3AtmosMeteringMode' "ITU_BS_1770_4"

pattern EAMMLeqA :: Eac3AtmosMeteringMode
pattern EAMMLeqA = Eac3AtmosMeteringMode' "LEQ_A"

{-# COMPLETE
  EAMMItuBs17701,
  EAMMItuBs17702,
  EAMMItuBs17703,
  EAMMItuBs17704,
  EAMMLeqA,
  Eac3AtmosMeteringMode' #-}

instance FromText Eac3AtmosMeteringMode where
    parser = (Eac3AtmosMeteringMode' . mk) <$> takeText

instance ToText Eac3AtmosMeteringMode where
    toText (Eac3AtmosMeteringMode' ci) = original ci

-- | Represents an enum of /known/ $Eac3AtmosMeteringMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Eac3AtmosMeteringMode where
    toEnum i = case i of
        0 -> EAMMItuBs17701
        1 -> EAMMItuBs17702
        2 -> EAMMItuBs17703
        3 -> EAMMItuBs17704
        4 -> EAMMLeqA
        _ -> (error . showText) $ "Unknown index for Eac3AtmosMeteringMode: " <> toText i
    fromEnum x = case x of
        EAMMItuBs17701 -> 0
        EAMMItuBs17702 -> 1
        EAMMItuBs17703 -> 2
        EAMMItuBs17704 -> 3
        EAMMLeqA -> 4
        Eac3AtmosMeteringMode' name -> (error . showText) $ "Unknown Eac3AtmosMeteringMode: " <> original name

-- | Represents the bounds of /known/ $Eac3AtmosMeteringMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Eac3AtmosMeteringMode where
    minBound = EAMMItuBs17701
    maxBound = EAMMLeqA

instance Hashable     Eac3AtmosMeteringMode
instance NFData       Eac3AtmosMeteringMode
instance ToByteString Eac3AtmosMeteringMode
instance ToQuery      Eac3AtmosMeteringMode
instance ToHeader     Eac3AtmosMeteringMode

instance ToJSON Eac3AtmosMeteringMode where
    toJSON = toJSONText

instance FromJSON Eac3AtmosMeteringMode where
    parseJSON = parseJSONText "Eac3AtmosMeteringMode"
