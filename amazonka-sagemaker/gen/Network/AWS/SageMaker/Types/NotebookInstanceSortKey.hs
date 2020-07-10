{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.NotebookInstanceSortKey
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.NotebookInstanceSortKey (
  NotebookInstanceSortKey (
    ..
    , NISKCreationTime
    , NISKName
    , NISKStatus
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data NotebookInstanceSortKey = NotebookInstanceSortKey' (CI
                                                           Text)
                                 deriving (Eq, Ord, Read, Show, Data, Typeable,
                                           Generic)

pattern NISKCreationTime :: NotebookInstanceSortKey
pattern NISKCreationTime = NotebookInstanceSortKey' "CreationTime"

pattern NISKName :: NotebookInstanceSortKey
pattern NISKName = NotebookInstanceSortKey' "Name"

pattern NISKStatus :: NotebookInstanceSortKey
pattern NISKStatus = NotebookInstanceSortKey' "Status"

{-# COMPLETE
  NISKCreationTime,
  NISKName,
  NISKStatus,
  NotebookInstanceSortKey' #-}

instance FromText NotebookInstanceSortKey where
    parser = (NotebookInstanceSortKey' . mk) <$> takeText

instance ToText NotebookInstanceSortKey where
    toText (NotebookInstanceSortKey' ci) = original ci

-- | Represents an enum of /known/ $NotebookInstanceSortKey.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum NotebookInstanceSortKey where
    toEnum i = case i of
        0 -> NISKCreationTime
        1 -> NISKName
        2 -> NISKStatus
        _ -> (error . showText) $ "Unknown index for NotebookInstanceSortKey: " <> toText i
    fromEnum x = case x of
        NISKCreationTime -> 0
        NISKName -> 1
        NISKStatus -> 2
        NotebookInstanceSortKey' name -> (error . showText) $ "Unknown NotebookInstanceSortKey: " <> original name

-- | Represents the bounds of /known/ $NotebookInstanceSortKey.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded NotebookInstanceSortKey where
    minBound = NISKCreationTime
    maxBound = NISKStatus

instance Hashable     NotebookInstanceSortKey
instance NFData       NotebookInstanceSortKey
instance ToByteString NotebookInstanceSortKey
instance ToQuery      NotebookInstanceSortKey
instance ToHeader     NotebookInstanceSortKey

instance ToJSON NotebookInstanceSortKey where
    toJSON = toJSONText
