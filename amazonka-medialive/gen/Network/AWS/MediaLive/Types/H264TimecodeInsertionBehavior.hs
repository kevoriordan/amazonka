{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H264TimecodeInsertionBehavior
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.H264TimecodeInsertionBehavior (
  H264TimecodeInsertionBehavior (
    ..
    , H26Disabled
    , H26PicTimingSei
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | H264 Timecode Insertion Behavior
data H264TimecodeInsertionBehavior = H264TimecodeInsertionBehavior' (CI
                                                                       Text)
                                       deriving (Eq, Ord, Read, Show, Data,
                                                 Typeable, Generic)

pattern H26Disabled :: H264TimecodeInsertionBehavior
pattern H26Disabled = H264TimecodeInsertionBehavior' "DISABLED"

pattern H26PicTimingSei :: H264TimecodeInsertionBehavior
pattern H26PicTimingSei = H264TimecodeInsertionBehavior' "PIC_TIMING_SEI"

{-# COMPLETE
  H26Disabled,
  H26PicTimingSei,
  H264TimecodeInsertionBehavior' #-}

instance FromText H264TimecodeInsertionBehavior where
    parser = (H264TimecodeInsertionBehavior' . mk) <$> takeText

instance ToText H264TimecodeInsertionBehavior where
    toText (H264TimecodeInsertionBehavior' ci) = original ci

-- | Represents an enum of /known/ $H264TimecodeInsertionBehavior.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum H264TimecodeInsertionBehavior where
    toEnum i = case i of
        0 -> H26Disabled
        1 -> H26PicTimingSei
        _ -> (error . showText) $ "Unknown index for H264TimecodeInsertionBehavior: " <> toText i
    fromEnum x = case x of
        H26Disabled -> 0
        H26PicTimingSei -> 1
        H264TimecodeInsertionBehavior' name -> (error . showText) $ "Unknown H264TimecodeInsertionBehavior: " <> original name

-- | Represents the bounds of /known/ $H264TimecodeInsertionBehavior.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded H264TimecodeInsertionBehavior where
    minBound = H26Disabled
    maxBound = H26PicTimingSei

instance Hashable     H264TimecodeInsertionBehavior
instance NFData       H264TimecodeInsertionBehavior
instance ToByteString H264TimecodeInsertionBehavior
instance ToQuery      H264TimecodeInsertionBehavior
instance ToHeader     H264TimecodeInsertionBehavior

instance ToJSON H264TimecodeInsertionBehavior where
    toJSON = toJSONText

instance FromJSON H264TimecodeInsertionBehavior where
    parseJSON = parseJSONText "H264TimecodeInsertionBehavior"
