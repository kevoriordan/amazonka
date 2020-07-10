{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.Crawler
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.Crawler where

import Network.AWS.Glue.Types.CrawlerState
import Network.AWS.Glue.Types.CrawlerTargets
import Network.AWS.Glue.Types.LastCrawlInfo
import Network.AWS.Glue.Types.Schedule
import Network.AWS.Glue.Types.SchemaChangePolicy
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies a crawler program that examines a data source and uses classifiers to try to determine its schema. If successful, the crawler records metadata concerning the data source in the AWS Glue Data Catalog.
--
--
--
-- /See:/ 'crawler' smart constructor.
data Crawler = Crawler'{_crwlrCreationTime ::
                        !(Maybe POSIX),
                        _crwlrState :: !(Maybe CrawlerState),
                        _crwlrSchemaChangePolicy ::
                        !(Maybe SchemaChangePolicy),
                        _crwlrLastUpdated :: !(Maybe POSIX),
                        _crwlrSchedule :: !(Maybe Schedule),
                        _crwlrLastCrawl :: !(Maybe LastCrawlInfo),
                        _crwlrCrawlElapsedTime :: !(Maybe Integer),
                        _crwlrClassifiers :: !(Maybe [Text]),
                        _crwlrRole :: !(Maybe Text),
                        _crwlrName :: !(Maybe Text),
                        _crwlrTargets :: !(Maybe CrawlerTargets),
                        _crwlrVersion :: !(Maybe Integer),
                        _crwlrDatabaseName :: !(Maybe Text),
                        _crwlrCrawlerSecurityConfiguration :: !(Maybe Text),
                        _crwlrConfiguration :: !(Maybe Text),
                        _crwlrTablePrefix :: !(Maybe Text),
                        _crwlrDescription :: !(Maybe Text)}
                 deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Crawler' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crwlrCreationTime' - The time that the crawler was created.
--
-- * 'crwlrState' - Indicates whether the crawler is running, or whether a run is pending.
--
-- * 'crwlrSchemaChangePolicy' - The policy that specifies update and delete behaviors for the crawler.
--
-- * 'crwlrLastUpdated' - The time that the crawler was last updated.
--
-- * 'crwlrSchedule' - For scheduled crawlers, the schedule when the crawler runs.
--
-- * 'crwlrLastCrawl' - The status of the last crawl, and potentially error information if an error occurred.
--
-- * 'crwlrCrawlElapsedTime' - If the crawler is running, contains the total time elapsed since the last crawl began.
--
-- * 'crwlrClassifiers' - A list of UTF-8 strings that specify the custom classifiers that are associated with the crawler.
--
-- * 'crwlrRole' - The Amazon Resource Name (ARN) of an IAM role that's used to access customer resources, such as Amazon Simple Storage Service (Amazon S3) data.
--
-- * 'crwlrName' - The name of the crawler.
--
-- * 'crwlrTargets' - A collection of targets to crawl.
--
-- * 'crwlrVersion' - The version of the crawler.
--
-- * 'crwlrDatabaseName' - The name of the database in which the crawler's output is stored.
--
-- * 'crwlrCrawlerSecurityConfiguration' - The name of the @SecurityConfiguration@ structure to be used by this crawler.
--
-- * 'crwlrConfiguration' - Crawler configuration information. This versioned JSON string allows users to specify aspects of a crawler's behavior. For more information, see <http://docs.aws.amazon.com/glue/latest/dg/crawler-configuration.html Configuring a Crawler> .
--
-- * 'crwlrTablePrefix' - The prefix added to the names of tables that are created.
--
-- * 'crwlrDescription' - A description of the crawler.
crawler
    :: Crawler
crawler
  = Crawler'{_crwlrCreationTime = Nothing,
             _crwlrState = Nothing,
             _crwlrSchemaChangePolicy = Nothing,
             _crwlrLastUpdated = Nothing,
             _crwlrSchedule = Nothing, _crwlrLastCrawl = Nothing,
             _crwlrCrawlElapsedTime = Nothing,
             _crwlrClassifiers = Nothing, _crwlrRole = Nothing,
             _crwlrName = Nothing, _crwlrTargets = Nothing,
             _crwlrVersion = Nothing,
             _crwlrDatabaseName = Nothing,
             _crwlrCrawlerSecurityConfiguration = Nothing,
             _crwlrConfiguration = Nothing,
             _crwlrTablePrefix = Nothing,
             _crwlrDescription = Nothing}

-- | The time that the crawler was created.
crwlrCreationTime :: Lens' Crawler (Maybe UTCTime)
crwlrCreationTime = lens _crwlrCreationTime (\ s a -> s{_crwlrCreationTime = a}) . mapping _Time

-- | Indicates whether the crawler is running, or whether a run is pending.
crwlrState :: Lens' Crawler (Maybe CrawlerState)
crwlrState = lens _crwlrState (\ s a -> s{_crwlrState = a})

-- | The policy that specifies update and delete behaviors for the crawler.
crwlrSchemaChangePolicy :: Lens' Crawler (Maybe SchemaChangePolicy)
crwlrSchemaChangePolicy = lens _crwlrSchemaChangePolicy (\ s a -> s{_crwlrSchemaChangePolicy = a})

-- | The time that the crawler was last updated.
crwlrLastUpdated :: Lens' Crawler (Maybe UTCTime)
crwlrLastUpdated = lens _crwlrLastUpdated (\ s a -> s{_crwlrLastUpdated = a}) . mapping _Time

-- | For scheduled crawlers, the schedule when the crawler runs.
crwlrSchedule :: Lens' Crawler (Maybe Schedule)
crwlrSchedule = lens _crwlrSchedule (\ s a -> s{_crwlrSchedule = a})

-- | The status of the last crawl, and potentially error information if an error occurred.
crwlrLastCrawl :: Lens' Crawler (Maybe LastCrawlInfo)
crwlrLastCrawl = lens _crwlrLastCrawl (\ s a -> s{_crwlrLastCrawl = a})

-- | If the crawler is running, contains the total time elapsed since the last crawl began.
crwlrCrawlElapsedTime :: Lens' Crawler (Maybe Integer)
crwlrCrawlElapsedTime = lens _crwlrCrawlElapsedTime (\ s a -> s{_crwlrCrawlElapsedTime = a})

-- | A list of UTF-8 strings that specify the custom classifiers that are associated with the crawler.
crwlrClassifiers :: Lens' Crawler [Text]
crwlrClassifiers = lens _crwlrClassifiers (\ s a -> s{_crwlrClassifiers = a}) . _Default . _Coerce

-- | The Amazon Resource Name (ARN) of an IAM role that's used to access customer resources, such as Amazon Simple Storage Service (Amazon S3) data.
crwlrRole :: Lens' Crawler (Maybe Text)
crwlrRole = lens _crwlrRole (\ s a -> s{_crwlrRole = a})

-- | The name of the crawler.
crwlrName :: Lens' Crawler (Maybe Text)
crwlrName = lens _crwlrName (\ s a -> s{_crwlrName = a})

-- | A collection of targets to crawl.
crwlrTargets :: Lens' Crawler (Maybe CrawlerTargets)
crwlrTargets = lens _crwlrTargets (\ s a -> s{_crwlrTargets = a})

-- | The version of the crawler.
crwlrVersion :: Lens' Crawler (Maybe Integer)
crwlrVersion = lens _crwlrVersion (\ s a -> s{_crwlrVersion = a})

-- | The name of the database in which the crawler's output is stored.
crwlrDatabaseName :: Lens' Crawler (Maybe Text)
crwlrDatabaseName = lens _crwlrDatabaseName (\ s a -> s{_crwlrDatabaseName = a})

-- | The name of the @SecurityConfiguration@ structure to be used by this crawler.
crwlrCrawlerSecurityConfiguration :: Lens' Crawler (Maybe Text)
crwlrCrawlerSecurityConfiguration = lens _crwlrCrawlerSecurityConfiguration (\ s a -> s{_crwlrCrawlerSecurityConfiguration = a})

-- | Crawler configuration information. This versioned JSON string allows users to specify aspects of a crawler's behavior. For more information, see <http://docs.aws.amazon.com/glue/latest/dg/crawler-configuration.html Configuring a Crawler> .
crwlrConfiguration :: Lens' Crawler (Maybe Text)
crwlrConfiguration = lens _crwlrConfiguration (\ s a -> s{_crwlrConfiguration = a})

-- | The prefix added to the names of tables that are created.
crwlrTablePrefix :: Lens' Crawler (Maybe Text)
crwlrTablePrefix = lens _crwlrTablePrefix (\ s a -> s{_crwlrTablePrefix = a})

-- | A description of the crawler.
crwlrDescription :: Lens' Crawler (Maybe Text)
crwlrDescription = lens _crwlrDescription (\ s a -> s{_crwlrDescription = a})

instance FromJSON Crawler where
        parseJSON
          = withObject "Crawler"
              (\ x ->
                 Crawler' <$>
                   (x .:? "CreationTime") <*> (x .:? "State") <*>
                     (x .:? "SchemaChangePolicy")
                     <*> (x .:? "LastUpdated")
                     <*> (x .:? "Schedule")
                     <*> (x .:? "LastCrawl")
                     <*> (x .:? "CrawlElapsedTime")
                     <*> (x .:? "Classifiers" .!= mempty)
                     <*> (x .:? "Role")
                     <*> (x .:? "Name")
                     <*> (x .:? "Targets")
                     <*> (x .:? "Version")
                     <*> (x .:? "DatabaseName")
                     <*> (x .:? "CrawlerSecurityConfiguration")
                     <*> (x .:? "Configuration")
                     <*> (x .:? "TablePrefix")
                     <*> (x .:? "Description"))

instance Hashable Crawler where

instance NFData Crawler where
