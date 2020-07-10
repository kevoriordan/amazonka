{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.NotebookOutputOption
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.NotebookOutputOption (
  NotebookOutputOption (
    ..
    , Allowed
    , Disabled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data NotebookOutputOption = NotebookOutputOption' (CI
                                                     Text)
                              deriving (Eq, Ord, Read, Show, Data, Typeable,
                                        Generic)

pattern Allowed :: NotebookOutputOption
pattern Allowed = NotebookOutputOption' "Allowed"

pattern Disabled :: NotebookOutputOption
pattern Disabled = NotebookOutputOption' "Disabled"

{-# COMPLETE
  Allowed,
  Disabled,
  NotebookOutputOption' #-}

instance FromText NotebookOutputOption where
    parser = (NotebookOutputOption' . mk) <$> takeText

instance ToText NotebookOutputOption where
    toText (NotebookOutputOption' ci) = original ci

-- | Represents an enum of /known/ $NotebookOutputOption.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum NotebookOutputOption where
    toEnum i = case i of
        0 -> Allowed
        1 -> Disabled
        _ -> (error . showText) $ "Unknown index for NotebookOutputOption: " <> toText i
    fromEnum x = case x of
        Allowed -> 0
        Disabled -> 1
        NotebookOutputOption' name -> (error . showText) $ "Unknown NotebookOutputOption: " <> original name

-- | Represents the bounds of /known/ $NotebookOutputOption.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded NotebookOutputOption where
    minBound = Allowed
    maxBound = Disabled

instance Hashable     NotebookOutputOption
instance NFData       NotebookOutputOption
instance ToByteString NotebookOutputOption
instance ToQuery      NotebookOutputOption
instance ToHeader     NotebookOutputOption

instance ToJSON NotebookOutputOption where
    toJSON = toJSONText

instance FromJSON NotebookOutputOption where
    parseJSON = parseJSONText "NotebookOutputOption"
