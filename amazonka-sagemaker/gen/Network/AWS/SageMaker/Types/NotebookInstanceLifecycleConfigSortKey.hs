{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.NotebookInstanceLifecycleConfigSortKey
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.NotebookInstanceLifecycleConfigSortKey (
  NotebookInstanceLifecycleConfigSortKey (
    ..
    , NILCSKCreationTime
    , NILCSKLastModifiedTime
    , NILCSKName
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data NotebookInstanceLifecycleConfigSortKey = NotebookInstanceLifecycleConfigSortKey' (CI
                                                                                         Text)
                                                deriving (Eq, Ord, Read, Show,
                                                          Data, Typeable,
                                                          Generic)

pattern NILCSKCreationTime :: NotebookInstanceLifecycleConfigSortKey
pattern NILCSKCreationTime = NotebookInstanceLifecycleConfigSortKey' "CreationTime"

pattern NILCSKLastModifiedTime :: NotebookInstanceLifecycleConfigSortKey
pattern NILCSKLastModifiedTime = NotebookInstanceLifecycleConfigSortKey' "LastModifiedTime"

pattern NILCSKName :: NotebookInstanceLifecycleConfigSortKey
pattern NILCSKName = NotebookInstanceLifecycleConfigSortKey' "Name"

{-# COMPLETE
  NILCSKCreationTime,
  NILCSKLastModifiedTime,
  NILCSKName,
  NotebookInstanceLifecycleConfigSortKey' #-}

instance FromText NotebookInstanceLifecycleConfigSortKey where
    parser = (NotebookInstanceLifecycleConfigSortKey' . mk) <$> takeText

instance ToText NotebookInstanceLifecycleConfigSortKey where
    toText (NotebookInstanceLifecycleConfigSortKey' ci) = original ci

-- | Represents an enum of /known/ $NotebookInstanceLifecycleConfigSortKey.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum NotebookInstanceLifecycleConfigSortKey where
    toEnum i = case i of
        0 -> NILCSKCreationTime
        1 -> NILCSKLastModifiedTime
        2 -> NILCSKName
        _ -> (error . showText) $ "Unknown index for NotebookInstanceLifecycleConfigSortKey: " <> toText i
    fromEnum x = case x of
        NILCSKCreationTime -> 0
        NILCSKLastModifiedTime -> 1
        NILCSKName -> 2
        NotebookInstanceLifecycleConfigSortKey' name -> (error . showText) $ "Unknown NotebookInstanceLifecycleConfigSortKey: " <> original name

-- | Represents the bounds of /known/ $NotebookInstanceLifecycleConfigSortKey.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded NotebookInstanceLifecycleConfigSortKey where
    minBound = NILCSKCreationTime
    maxBound = NILCSKName

instance Hashable     NotebookInstanceLifecycleConfigSortKey
instance NFData       NotebookInstanceLifecycleConfigSortKey
instance ToByteString NotebookInstanceLifecycleConfigSortKey
instance ToQuery      NotebookInstanceLifecycleConfigSortKey
instance ToHeader     NotebookInstanceLifecycleConfigSortKey

instance ToJSON NotebookInstanceLifecycleConfigSortKey where
    toJSON = toJSONText
