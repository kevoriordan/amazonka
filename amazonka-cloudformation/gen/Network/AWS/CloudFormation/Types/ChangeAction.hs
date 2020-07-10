{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.ChangeAction
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFormation.Types.ChangeAction (
  ChangeAction (
    ..
    , Add
    , Import
    , Modify
    , Remove
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ChangeAction = ChangeAction' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern Add :: ChangeAction
pattern Add = ChangeAction' "Add"

pattern Import :: ChangeAction
pattern Import = ChangeAction' "Import"

pattern Modify :: ChangeAction
pattern Modify = ChangeAction' "Modify"

pattern Remove :: ChangeAction
pattern Remove = ChangeAction' "Remove"

{-# COMPLETE
  Add,
  Import,
  Modify,
  Remove,
  ChangeAction' #-}

instance FromText ChangeAction where
    parser = (ChangeAction' . mk) <$> takeText

instance ToText ChangeAction where
    toText (ChangeAction' ci) = original ci

-- | Represents an enum of /known/ $ChangeAction.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ChangeAction where
    toEnum i = case i of
        0 -> Add
        1 -> Import
        2 -> Modify
        3 -> Remove
        _ -> (error . showText) $ "Unknown index for ChangeAction: " <> toText i
    fromEnum x = case x of
        Add -> 0
        Import -> 1
        Modify -> 2
        Remove -> 3
        ChangeAction' name -> (error . showText) $ "Unknown ChangeAction: " <> original name

-- | Represents the bounds of /known/ $ChangeAction.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ChangeAction where
    minBound = Add
    maxBound = Remove

instance Hashable     ChangeAction
instance NFData       ChangeAction
instance ToByteString ChangeAction
instance ToQuery      ChangeAction
instance ToHeader     ChangeAction

instance FromXML ChangeAction where
    parseXML = parseXMLText "ChangeAction"
