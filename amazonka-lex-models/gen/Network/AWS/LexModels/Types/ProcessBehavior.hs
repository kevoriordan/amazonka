{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.ProcessBehavior
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.LexModels.Types.ProcessBehavior (
  ProcessBehavior (
    ..
    , Build
    , Save
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ProcessBehavior = ProcessBehavior' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern Build :: ProcessBehavior
pattern Build = ProcessBehavior' "BUILD"

pattern Save :: ProcessBehavior
pattern Save = ProcessBehavior' "SAVE"

{-# COMPLETE
  Build,
  Save,
  ProcessBehavior' #-}

instance FromText ProcessBehavior where
    parser = (ProcessBehavior' . mk) <$> takeText

instance ToText ProcessBehavior where
    toText (ProcessBehavior' ci) = original ci

-- | Represents an enum of /known/ $ProcessBehavior.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ProcessBehavior where
    toEnum i = case i of
        0 -> Build
        1 -> Save
        _ -> (error . showText) $ "Unknown index for ProcessBehavior: " <> toText i
    fromEnum x = case x of
        Build -> 0
        Save -> 1
        ProcessBehavior' name -> (error . showText) $ "Unknown ProcessBehavior: " <> original name

-- | Represents the bounds of /known/ $ProcessBehavior.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ProcessBehavior where
    minBound = Build
    maxBound = Save

instance Hashable     ProcessBehavior
instance NFData       ProcessBehavior
instance ToByteString ProcessBehavior
instance ToQuery      ProcessBehavior
instance ToHeader     ProcessBehavior

instance ToJSON ProcessBehavior where
    toJSON = toJSONText
