{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.OutputSdt
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.OutputSdt (
  OutputSdt (
    ..
    , SdtFollow
    , SdtFollowIfPresent
    , SdtManual
    , SdtNone
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Selects method of inserting SDT information into output stream.  "Follow input SDT" copies SDT information from input stream to  output stream. "Follow input SDT if present" copies SDT information from  input stream to output stream if SDT information is present in the input, otherwise it will fall back on the user-defined values. Enter "SDT  Manually" means user will enter the SDT information. "No SDT" means output  stream will not contain SDT information.
data OutputSdt = OutputSdt' (CI Text)
                   deriving (Eq, Ord, Read, Show, Data, Typeable,
                             Generic)

pattern SdtFollow :: OutputSdt
pattern SdtFollow = OutputSdt' "SDT_FOLLOW"

pattern SdtFollowIfPresent :: OutputSdt
pattern SdtFollowIfPresent = OutputSdt' "SDT_FOLLOW_IF_PRESENT"

pattern SdtManual :: OutputSdt
pattern SdtManual = OutputSdt' "SDT_MANUAL"

pattern SdtNone :: OutputSdt
pattern SdtNone = OutputSdt' "SDT_NONE"

{-# COMPLETE
  SdtFollow,
  SdtFollowIfPresent,
  SdtManual,
  SdtNone,
  OutputSdt' #-}

instance FromText OutputSdt where
    parser = (OutputSdt' . mk) <$> takeText

instance ToText OutputSdt where
    toText (OutputSdt' ci) = original ci

-- | Represents an enum of /known/ $OutputSdt.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum OutputSdt where
    toEnum i = case i of
        0 -> SdtFollow
        1 -> SdtFollowIfPresent
        2 -> SdtManual
        3 -> SdtNone
        _ -> (error . showText) $ "Unknown index for OutputSdt: " <> toText i
    fromEnum x = case x of
        SdtFollow -> 0
        SdtFollowIfPresent -> 1
        SdtManual -> 2
        SdtNone -> 3
        OutputSdt' name -> (error . showText) $ "Unknown OutputSdt: " <> original name

-- | Represents the bounds of /known/ $OutputSdt.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded OutputSdt where
    minBound = SdtFollow
    maxBound = SdtNone

instance Hashable     OutputSdt
instance NFData       OutputSdt
instance ToByteString OutputSdt
instance ToQuery      OutputSdt
instance ToHeader     OutputSdt

instance ToJSON OutputSdt where
    toJSON = toJSONText

instance FromJSON OutputSdt where
    parseJSON = parseJSONText "OutputSdt"
