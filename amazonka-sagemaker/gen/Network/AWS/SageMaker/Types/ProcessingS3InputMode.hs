{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ProcessingS3InputMode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.ProcessingS3InputMode (
  ProcessingS3InputMode (
    ..
    , PSIMFile
    , PSIMPipe
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ProcessingS3InputMode = ProcessingS3InputMode' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern PSIMFile :: ProcessingS3InputMode
pattern PSIMFile = ProcessingS3InputMode' "File"

pattern PSIMPipe :: ProcessingS3InputMode
pattern PSIMPipe = ProcessingS3InputMode' "Pipe"

{-# COMPLETE
  PSIMFile,
  PSIMPipe,
  ProcessingS3InputMode' #-}

instance FromText ProcessingS3InputMode where
    parser = (ProcessingS3InputMode' . mk) <$> takeText

instance ToText ProcessingS3InputMode where
    toText (ProcessingS3InputMode' ci) = original ci

-- | Represents an enum of /known/ $ProcessingS3InputMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ProcessingS3InputMode where
    toEnum i = case i of
        0 -> PSIMFile
        1 -> PSIMPipe
        _ -> (error . showText) $ "Unknown index for ProcessingS3InputMode: " <> toText i
    fromEnum x = case x of
        PSIMFile -> 0
        PSIMPipe -> 1
        ProcessingS3InputMode' name -> (error . showText) $ "Unknown ProcessingS3InputMode: " <> original name

-- | Represents the bounds of /known/ $ProcessingS3InputMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ProcessingS3InputMode where
    minBound = PSIMFile
    maxBound = PSIMPipe

instance Hashable     ProcessingS3InputMode
instance NFData       ProcessingS3InputMode
instance ToByteString ProcessingS3InputMode
instance ToQuery      ProcessingS3InputMode
instance ToHeader     ProcessingS3InputMode

instance ToJSON ProcessingS3InputMode where
    toJSON = toJSONText

instance FromJSON ProcessingS3InputMode where
    parseJSON = parseJSONText "ProcessingS3InputMode"
