{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Types.StatementType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Athena.Types.StatementType (
  StatementType (
    ..
    , Ddl
    , Dml
    , Utility
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data StatementType = StatementType' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern Ddl :: StatementType
pattern Ddl = StatementType' "DDL"

pattern Dml :: StatementType
pattern Dml = StatementType' "DML"

pattern Utility :: StatementType
pattern Utility = StatementType' "UTILITY"

{-# COMPLETE
  Ddl,
  Dml,
  Utility,
  StatementType' #-}

instance FromText StatementType where
    parser = (StatementType' . mk) <$> takeText

instance ToText StatementType where
    toText (StatementType' ci) = original ci

-- | Represents an enum of /known/ $StatementType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum StatementType where
    toEnum i = case i of
        0 -> Ddl
        1 -> Dml
        2 -> Utility
        _ -> (error . showText) $ "Unknown index for StatementType: " <> toText i
    fromEnum x = case x of
        Ddl -> 0
        Dml -> 1
        Utility -> 2
        StatementType' name -> (error . showText) $ "Unknown StatementType: " <> original name

-- | Represents the bounds of /known/ $StatementType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded StatementType where
    minBound = Ddl
    maxBound = Utility

instance Hashable     StatementType
instance NFData       StatementType
instance ToByteString StatementType
instance ToQuery      StatementType
instance ToHeader     StatementType

instance FromJSON StatementType where
    parseJSON = parseJSONText "StatementType"
