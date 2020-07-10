{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ExpressionType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.ExpressionType (
  ExpressionType (
    ..
    , Sql
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude
import Network.AWS.S3.Internal

data ExpressionType = ExpressionType' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern Sql :: ExpressionType
pattern Sql = ExpressionType' "SQL"

{-# COMPLETE
  Sql,
  ExpressionType' #-}

instance FromText ExpressionType where
    parser = (ExpressionType' . mk) <$> takeText

instance ToText ExpressionType where
    toText (ExpressionType' ci) = original ci

-- | Represents an enum of /known/ $ExpressionType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ExpressionType where
    toEnum i = case i of
        0 -> Sql
        _ -> (error . showText) $ "Unknown index for ExpressionType: " <> toText i
    fromEnum x = case x of
        Sql -> 0
        ExpressionType' name -> (error . showText) $ "Unknown ExpressionType: " <> original name

-- | Represents the bounds of /known/ $ExpressionType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ExpressionType where
    minBound = Sql
    maxBound = Sql

instance Hashable     ExpressionType
instance NFData       ExpressionType
instance ToByteString ExpressionType
instance ToQuery      ExpressionType
instance ToHeader     ExpressionType

instance ToXML ExpressionType where
    toXML = toXMLText
