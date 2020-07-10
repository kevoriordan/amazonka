{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.RecordType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53.Types.RecordType (
  RecordType (
    ..
    , A
    , Aaaa
    , Caa
    , Cname
    , MX
    , NS
    , Naptr
    , Ptr
    , Soa
    , Spf
    , Srv
    , Txt
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude
import Network.AWS.Route53.Internal

data RecordType = RecordType' (CI Text)
                    deriving (Eq, Ord, Read, Show, Data, Typeable,
                              Generic)

pattern A :: RecordType
pattern A = RecordType' "A"

pattern Aaaa :: RecordType
pattern Aaaa = RecordType' "AAAA"

pattern Caa :: RecordType
pattern Caa = RecordType' "CAA"

pattern Cname :: RecordType
pattern Cname = RecordType' "CNAME"

pattern MX :: RecordType
pattern MX = RecordType' "MX"

pattern NS :: RecordType
pattern NS = RecordType' "NS"

pattern Naptr :: RecordType
pattern Naptr = RecordType' "NAPTR"

pattern Ptr :: RecordType
pattern Ptr = RecordType' "PTR"

pattern Soa :: RecordType
pattern Soa = RecordType' "SOA"

pattern Spf :: RecordType
pattern Spf = RecordType' "SPF"

pattern Srv :: RecordType
pattern Srv = RecordType' "SRV"

pattern Txt :: RecordType
pattern Txt = RecordType' "TXT"

{-# COMPLETE
  A,
  Aaaa,
  Caa,
  Cname,
  MX,
  NS,
  Naptr,
  Ptr,
  Soa,
  Spf,
  Srv,
  Txt,
  RecordType' #-}

instance FromText RecordType where
    parser = (RecordType' . mk) <$> takeText

instance ToText RecordType where
    toText (RecordType' ci) = original ci

-- | Represents an enum of /known/ $RecordType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum RecordType where
    toEnum i = case i of
        0 -> A
        1 -> Aaaa
        2 -> Caa
        3 -> Cname
        4 -> MX
        5 -> NS
        6 -> Naptr
        7 -> Ptr
        8 -> Soa
        9 -> Spf
        10 -> Srv
        11 -> Txt
        _ -> (error . showText) $ "Unknown index for RecordType: " <> toText i
    fromEnum x = case x of
        A -> 0
        Aaaa -> 1
        Caa -> 2
        Cname -> 3
        MX -> 4
        NS -> 5
        Naptr -> 6
        Ptr -> 7
        Soa -> 8
        Spf -> 9
        Srv -> 10
        Txt -> 11
        RecordType' name -> (error . showText) $ "Unknown RecordType: " <> original name

-- | Represents the bounds of /known/ $RecordType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded RecordType where
    minBound = A
    maxBound = Txt

instance Hashable     RecordType
instance NFData       RecordType
instance ToByteString RecordType
instance ToQuery      RecordType
instance ToHeader     RecordType

instance FromXML RecordType where
    parseXML = parseXMLText "RecordType"

instance ToXML RecordType where
    toXML = toXMLText
