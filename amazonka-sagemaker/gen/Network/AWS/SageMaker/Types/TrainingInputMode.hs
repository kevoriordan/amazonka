{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TrainingInputMode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.TrainingInputMode (
  TrainingInputMode (
    ..
    , File
    , Pipe
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data TrainingInputMode = TrainingInputMode' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern File :: TrainingInputMode
pattern File = TrainingInputMode' "File"

pattern Pipe :: TrainingInputMode
pattern Pipe = TrainingInputMode' "Pipe"

{-# COMPLETE
  File,
  Pipe,
  TrainingInputMode' #-}

instance FromText TrainingInputMode where
    parser = (TrainingInputMode' . mk) <$> takeText

instance ToText TrainingInputMode where
    toText (TrainingInputMode' ci) = original ci

-- | Represents an enum of /known/ $TrainingInputMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum TrainingInputMode where
    toEnum i = case i of
        0 -> File
        1 -> Pipe
        _ -> (error . showText) $ "Unknown index for TrainingInputMode: " <> toText i
    fromEnum x = case x of
        File -> 0
        Pipe -> 1
        TrainingInputMode' name -> (error . showText) $ "Unknown TrainingInputMode: " <> original name

-- | Represents the bounds of /known/ $TrainingInputMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded TrainingInputMode where
    minBound = File
    maxBound = Pipe

instance Hashable     TrainingInputMode
instance NFData       TrainingInputMode
instance ToByteString TrainingInputMode
instance ToQuery      TrainingInputMode
instance ToHeader     TrainingInputMode

instance ToJSON TrainingInputMode where
    toJSON = toJSONText

instance FromJSON TrainingInputMode where
    parseJSON = parseJSONText "TrainingInputMode"
