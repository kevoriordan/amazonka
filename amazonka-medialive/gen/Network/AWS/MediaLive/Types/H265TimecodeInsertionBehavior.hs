{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H265TimecodeInsertionBehavior
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.H265TimecodeInsertionBehavior (
  H265TimecodeInsertionBehavior (
    ..
    , HTIBDisabled
    , HTIBPicTimingSei
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | H265 Timecode Insertion Behavior
data H265TimecodeInsertionBehavior = H265TimecodeInsertionBehavior' (CI
                                                                       Text)
                                       deriving (Eq, Ord, Read, Show, Data,
                                                 Typeable, Generic)

pattern HTIBDisabled :: H265TimecodeInsertionBehavior
pattern HTIBDisabled = H265TimecodeInsertionBehavior' "DISABLED"

pattern HTIBPicTimingSei :: H265TimecodeInsertionBehavior
pattern HTIBPicTimingSei = H265TimecodeInsertionBehavior' "PIC_TIMING_SEI"

{-# COMPLETE
  HTIBDisabled,
  HTIBPicTimingSei,
  H265TimecodeInsertionBehavior' #-}

instance FromText H265TimecodeInsertionBehavior where
    parser = (H265TimecodeInsertionBehavior' . mk) <$> takeText

instance ToText H265TimecodeInsertionBehavior where
    toText (H265TimecodeInsertionBehavior' ci) = original ci

-- | Represents an enum of /known/ $H265TimecodeInsertionBehavior.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum H265TimecodeInsertionBehavior where
    toEnum i = case i of
        0 -> HTIBDisabled
        1 -> HTIBPicTimingSei
        _ -> (error . showText) $ "Unknown index for H265TimecodeInsertionBehavior: " <> toText i
    fromEnum x = case x of
        HTIBDisabled -> 0
        HTIBPicTimingSei -> 1
        H265TimecodeInsertionBehavior' name -> (error . showText) $ "Unknown H265TimecodeInsertionBehavior: " <> original name

-- | Represents the bounds of /known/ $H265TimecodeInsertionBehavior.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded H265TimecodeInsertionBehavior where
    minBound = HTIBDisabled
    maxBound = HTIBPicTimingSei

instance Hashable     H265TimecodeInsertionBehavior
instance NFData       H265TimecodeInsertionBehavior
instance ToByteString H265TimecodeInsertionBehavior
instance ToQuery      H265TimecodeInsertionBehavior
instance ToHeader     H265TimecodeInsertionBehavior

instance ToJSON H265TimecodeInsertionBehavior where
    toJSON = toJSONText

instance FromJSON H265TimecodeInsertionBehavior where
    parseJSON = parseJSONText "H265TimecodeInsertionBehavior"
