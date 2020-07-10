{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.PublicKeySummary
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types.PublicKeySummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A complex data type for public key information. 
--
--
--
-- /See:/ 'publicKeySummary' smart constructor.
data PublicKeySummary = PublicKeySummary'{_pksComment
                                          :: !(Maybe Text),
                                          _pksId :: !Text, _pksName :: !Text,
                                          _pksCreatedTime :: !ISO8601,
                                          _pksEncodedKey :: !Text}
                          deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PublicKeySummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pksComment' - Comment for public key information summary. 
--
-- * 'pksId' - ID for public key information summary. 
--
-- * 'pksName' - Name for public key information summary. 
--
-- * 'pksCreatedTime' - Creation time for public key information summary. 
--
-- * 'pksEncodedKey' - Encoded key for public key information summary. 
publicKeySummary
    :: Text -- ^ 'pksId'
    -> Text -- ^ 'pksName'
    -> UTCTime -- ^ 'pksCreatedTime'
    -> Text -- ^ 'pksEncodedKey'
    -> PublicKeySummary
publicKeySummary pId_ pName_ pCreatedTime_
  pEncodedKey_
  = PublicKeySummary'{_pksComment = Nothing,
                      _pksId = pId_, _pksName = pName_,
                      _pksCreatedTime = _Time # pCreatedTime_,
                      _pksEncodedKey = pEncodedKey_}

-- | Comment for public key information summary. 
pksComment :: Lens' PublicKeySummary (Maybe Text)
pksComment = lens _pksComment (\ s a -> s{_pksComment = a})

-- | ID for public key information summary. 
pksId :: Lens' PublicKeySummary Text
pksId = lens _pksId (\ s a -> s{_pksId = a})

-- | Name for public key information summary. 
pksName :: Lens' PublicKeySummary Text
pksName = lens _pksName (\ s a -> s{_pksName = a})

-- | Creation time for public key information summary. 
pksCreatedTime :: Lens' PublicKeySummary UTCTime
pksCreatedTime = lens _pksCreatedTime (\ s a -> s{_pksCreatedTime = a}) . _Time

-- | Encoded key for public key information summary. 
pksEncodedKey :: Lens' PublicKeySummary Text
pksEncodedKey = lens _pksEncodedKey (\ s a -> s{_pksEncodedKey = a})

instance FromXML PublicKeySummary where
        parseXML x
          = PublicKeySummary' <$>
              (x .@? "Comment") <*> (x .@ "Id") <*> (x .@ "Name")
                <*> (x .@ "CreatedTime")
                <*> (x .@ "EncodedKey")

instance Hashable PublicKeySummary where

instance NFData PublicKeySummary where
