{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.Action
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AppStream.Types.Action (
  Action (
    ..
    , ClipboardCopyFromLocalDevice
    , ClipboardCopyToLocalDevice
    , FileDownload
    , FileUpload
    , PrintingToLocalDevice
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data Action = Action' (CI Text)
                deriving (Eq, Ord, Read, Show, Data, Typeable,
                          Generic)

pattern ClipboardCopyFromLocalDevice :: Action
pattern ClipboardCopyFromLocalDevice = Action' "CLIPBOARD_COPY_FROM_LOCAL_DEVICE"

pattern ClipboardCopyToLocalDevice :: Action
pattern ClipboardCopyToLocalDevice = Action' "CLIPBOARD_COPY_TO_LOCAL_DEVICE"

pattern FileDownload :: Action
pattern FileDownload = Action' "FILE_DOWNLOAD"

pattern FileUpload :: Action
pattern FileUpload = Action' "FILE_UPLOAD"

pattern PrintingToLocalDevice :: Action
pattern PrintingToLocalDevice = Action' "PRINTING_TO_LOCAL_DEVICE"

{-# COMPLETE
  ClipboardCopyFromLocalDevice,
  ClipboardCopyToLocalDevice,
  FileDownload,
  FileUpload,
  PrintingToLocalDevice,
  Action' #-}

instance FromText Action where
    parser = (Action' . mk) <$> takeText

instance ToText Action where
    toText (Action' ci) = original ci

-- | Represents an enum of /known/ $Action.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Action where
    toEnum i = case i of
        0 -> ClipboardCopyFromLocalDevice
        1 -> ClipboardCopyToLocalDevice
        2 -> FileDownload
        3 -> FileUpload
        4 -> PrintingToLocalDevice
        _ -> (error . showText) $ "Unknown index for Action: " <> toText i
    fromEnum x = case x of
        ClipboardCopyFromLocalDevice -> 0
        ClipboardCopyToLocalDevice -> 1
        FileDownload -> 2
        FileUpload -> 3
        PrintingToLocalDevice -> 4
        Action' name -> (error . showText) $ "Unknown Action: " <> original name

-- | Represents the bounds of /known/ $Action.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Action where
    minBound = ClipboardCopyFromLocalDevice
    maxBound = PrintingToLocalDevice

instance Hashable     Action
instance NFData       Action
instance ToByteString Action
instance ToQuery      Action
instance ToHeader     Action

instance ToJSON Action where
    toJSON = toJSONText

instance FromJSON Action where
    parseJSON = parseJSONText "Action"
