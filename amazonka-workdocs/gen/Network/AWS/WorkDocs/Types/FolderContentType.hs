{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.FolderContentType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkDocs.Types.FolderContentType (
  FolderContentType (
    ..
    , FCTAll
    , FCTDocument
    , FCTFolder
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data FolderContentType = FolderContentType' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern FCTAll :: FolderContentType
pattern FCTAll = FolderContentType' "ALL"

pattern FCTDocument :: FolderContentType
pattern FCTDocument = FolderContentType' "DOCUMENT"

pattern FCTFolder :: FolderContentType
pattern FCTFolder = FolderContentType' "FOLDER"

{-# COMPLETE
  FCTAll,
  FCTDocument,
  FCTFolder,
  FolderContentType' #-}

instance FromText FolderContentType where
    parser = (FolderContentType' . mk) <$> takeText

instance ToText FolderContentType where
    toText (FolderContentType' ci) = original ci

-- | Represents an enum of /known/ $FolderContentType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum FolderContentType where
    toEnum i = case i of
        0 -> FCTAll
        1 -> FCTDocument
        2 -> FCTFolder
        _ -> (error . showText) $ "Unknown index for FolderContentType: " <> toText i
    fromEnum x = case x of
        FCTAll -> 0
        FCTDocument -> 1
        FCTFolder -> 2
        FolderContentType' name -> (error . showText) $ "Unknown FolderContentType: " <> original name

-- | Represents the bounds of /known/ $FolderContentType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded FolderContentType where
    minBound = FCTAll
    maxBound = FCTFolder

instance Hashable     FolderContentType
instance NFData       FolderContentType
instance ToByteString FolderContentType
instance ToQuery      FolderContentType
instance ToHeader     FolderContentType

instance ToJSON FolderContentType where
    toJSON = toJSONText
