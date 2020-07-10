{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.CSVClassifier
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.CSVClassifier where

import Network.AWS.Glue.Types.CSVHeaderOption
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A classifier for custom @CSV@ content.
--
--
--
-- /See:/ 'csvClassifier' smart constructor.
data CSVClassifier = CSVClassifier'{_ccCreationTime
                                    :: !(Maybe POSIX),
                                    _ccQuoteSymbol :: !(Maybe Text),
                                    _ccContainsHeader ::
                                    !(Maybe CSVHeaderOption),
                                    _ccLastUpdated :: !(Maybe POSIX),
                                    _ccDisableValueTrimming :: !(Maybe Bool),
                                    _ccHeader :: !(Maybe [Text]),
                                    _ccVersion :: !(Maybe Integer),
                                    _ccAllowSingleColumn :: !(Maybe Bool),
                                    _ccDelimiter :: !(Maybe Text),
                                    _ccName :: !Text}
                       deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CSVClassifier' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccCreationTime' - The time that this classifier was registered.
--
-- * 'ccQuoteSymbol' - A custom symbol to denote what combines content into a single column value. It must be different from the column delimiter.
--
-- * 'ccContainsHeader' - Indicates whether the CSV file contains a header.
--
-- * 'ccLastUpdated' - The time that this classifier was last updated.
--
-- * 'ccDisableValueTrimming' - Specifies not to trim values before identifying the type of column values. The default value is @true@ .
--
-- * 'ccHeader' - A list of strings representing column names.
--
-- * 'ccVersion' - The version of this classifier.
--
-- * 'ccAllowSingleColumn' - Enables the processing of files that contain only one column.
--
-- * 'ccDelimiter' - A custom symbol to denote what separates each column entry in the row.
--
-- * 'ccName' - The name of the classifier.
csvClassifier
    :: Text -- ^ 'ccName'
    -> CSVClassifier
csvClassifier pName_
  = CSVClassifier'{_ccCreationTime = Nothing,
                   _ccQuoteSymbol = Nothing,
                   _ccContainsHeader = Nothing,
                   _ccLastUpdated = Nothing,
                   _ccDisableValueTrimming = Nothing,
                   _ccHeader = Nothing, _ccVersion = Nothing,
                   _ccAllowSingleColumn = Nothing,
                   _ccDelimiter = Nothing, _ccName = pName_}

-- | The time that this classifier was registered.
ccCreationTime :: Lens' CSVClassifier (Maybe UTCTime)
ccCreationTime = lens _ccCreationTime (\ s a -> s{_ccCreationTime = a}) . mapping _Time

-- | A custom symbol to denote what combines content into a single column value. It must be different from the column delimiter.
ccQuoteSymbol :: Lens' CSVClassifier (Maybe Text)
ccQuoteSymbol = lens _ccQuoteSymbol (\ s a -> s{_ccQuoteSymbol = a})

-- | Indicates whether the CSV file contains a header.
ccContainsHeader :: Lens' CSVClassifier (Maybe CSVHeaderOption)
ccContainsHeader = lens _ccContainsHeader (\ s a -> s{_ccContainsHeader = a})

-- | The time that this classifier was last updated.
ccLastUpdated :: Lens' CSVClassifier (Maybe UTCTime)
ccLastUpdated = lens _ccLastUpdated (\ s a -> s{_ccLastUpdated = a}) . mapping _Time

-- | Specifies not to trim values before identifying the type of column values. The default value is @true@ .
ccDisableValueTrimming :: Lens' CSVClassifier (Maybe Bool)
ccDisableValueTrimming = lens _ccDisableValueTrimming (\ s a -> s{_ccDisableValueTrimming = a})

-- | A list of strings representing column names.
ccHeader :: Lens' CSVClassifier [Text]
ccHeader = lens _ccHeader (\ s a -> s{_ccHeader = a}) . _Default . _Coerce

-- | The version of this classifier.
ccVersion :: Lens' CSVClassifier (Maybe Integer)
ccVersion = lens _ccVersion (\ s a -> s{_ccVersion = a})

-- | Enables the processing of files that contain only one column.
ccAllowSingleColumn :: Lens' CSVClassifier (Maybe Bool)
ccAllowSingleColumn = lens _ccAllowSingleColumn (\ s a -> s{_ccAllowSingleColumn = a})

-- | A custom symbol to denote what separates each column entry in the row.
ccDelimiter :: Lens' CSVClassifier (Maybe Text)
ccDelimiter = lens _ccDelimiter (\ s a -> s{_ccDelimiter = a})

-- | The name of the classifier.
ccName :: Lens' CSVClassifier Text
ccName = lens _ccName (\ s a -> s{_ccName = a})

instance FromJSON CSVClassifier where
        parseJSON
          = withObject "CSVClassifier"
              (\ x ->
                 CSVClassifier' <$>
                   (x .:? "CreationTime") <*> (x .:? "QuoteSymbol") <*>
                     (x .:? "ContainsHeader")
                     <*> (x .:? "LastUpdated")
                     <*> (x .:? "DisableValueTrimming")
                     <*> (x .:? "Header" .!= mempty)
                     <*> (x .:? "Version")
                     <*> (x .:? "AllowSingleColumn")
                     <*> (x .:? "Delimiter")
                     <*> (x .: "Name"))

instance Hashable CSVClassifier where

instance NFData CSVClassifier where
