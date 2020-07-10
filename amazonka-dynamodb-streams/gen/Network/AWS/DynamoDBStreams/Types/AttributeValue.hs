{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDBStreams.Types.AttributeValue
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDBStreams.Types.AttributeValue where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the data for an attribute. You can set one, and only one, of the elements.
--
--
-- Each attribute in an item is a name-value pair. An attribute can be single-valued or multi-valued set. For example, a book item can have title and authors attributes. Each book has one title but can have many authors. The multi-valued attribute is a set; duplicate values are not allowed.
--
--
-- /See:/ 'attributeValue' smart constructor.
data AttributeValue = AttributeValue'{_avL ::
                                      !(Maybe [AttributeValue]),
                                      _avNS :: !(Maybe [Text]),
                                      _avM ::
                                      !(Maybe (Map Text AttributeValue)),
                                      _avNULL :: !(Maybe Bool),
                                      _avN :: !(Maybe Text),
                                      _avBS :: !(Maybe [Base64]),
                                      _avB :: !(Maybe Base64),
                                      _avSS :: !(Maybe [Text]),
                                      _avS :: !(Maybe Text),
                                      _avBOOL :: !(Maybe Bool)}
                        deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AttributeValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'avL' - A List data type.
--
-- * 'avNS' - A Number Set data type.
--
-- * 'avM' - A Map data type.
--
-- * 'avNULL' - A Null data type.
--
-- * 'avN' - A Number data type.
--
-- * 'avBS' - A Binary Set data type.
--
-- * 'avB' - A Binary data type.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
--
-- * 'avSS' - A String Set data type.
--
-- * 'avS' - A String data type.
--
-- * 'avBOOL' - A Boolean data type.
attributeValue
    :: AttributeValue
attributeValue
  = AttributeValue'{_avL = Nothing, _avNS = Nothing,
                    _avM = Nothing, _avNULL = Nothing, _avN = Nothing,
                    _avBS = Nothing, _avB = Nothing, _avSS = Nothing,
                    _avS = Nothing, _avBOOL = Nothing}

-- | A List data type.
avL :: Lens' AttributeValue [AttributeValue]
avL = lens _avL (\ s a -> s{_avL = a}) . _Default . _Coerce

-- | A Number Set data type.
avNS :: Lens' AttributeValue [Text]
avNS = lens _avNS (\ s a -> s{_avNS = a}) . _Default . _Coerce

-- | A Map data type.
avM :: Lens' AttributeValue (HashMap Text AttributeValue)
avM = lens _avM (\ s a -> s{_avM = a}) . _Default . _Map

-- | A Null data type.
avNULL :: Lens' AttributeValue (Maybe Bool)
avNULL = lens _avNULL (\ s a -> s{_avNULL = a})

-- | A Number data type.
avN :: Lens' AttributeValue (Maybe Text)
avN = lens _avN (\ s a -> s{_avN = a})

-- | A Binary Set data type.
avBS :: Lens' AttributeValue [ByteString]
avBS = lens _avBS (\ s a -> s{_avBS = a}) . _Default . _Coerce

-- | A Binary data type.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
avB :: Lens' AttributeValue (Maybe ByteString)
avB = lens _avB (\ s a -> s{_avB = a}) . mapping _Base64

-- | A String Set data type.
avSS :: Lens' AttributeValue [Text]
avSS = lens _avSS (\ s a -> s{_avSS = a}) . _Default . _Coerce

-- | A String data type.
avS :: Lens' AttributeValue (Maybe Text)
avS = lens _avS (\ s a -> s{_avS = a})

-- | A Boolean data type.
avBOOL :: Lens' AttributeValue (Maybe Bool)
avBOOL = lens _avBOOL (\ s a -> s{_avBOOL = a})

instance FromJSON AttributeValue where
        parseJSON
          = withObject "AttributeValue"
              (\ x ->
                 AttributeValue' <$>
                   (x .:? "L" .!= mempty) <*> (x .:? "NS" .!= mempty)
                     <*> (x .:? "M" .!= mempty)
                     <*> (x .:? "NULL")
                     <*> (x .:? "N")
                     <*> (x .:? "BS" .!= mempty)
                     <*> (x .:? "B")
                     <*> (x .:? "SS" .!= mempty)
                     <*> (x .:? "S")
                     <*> (x .:? "BOOL"))

instance Hashable AttributeValue where

instance NFData AttributeValue where
