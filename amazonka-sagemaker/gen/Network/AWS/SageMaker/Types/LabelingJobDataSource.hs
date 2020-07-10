{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.LabelingJobDataSource
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.LabelingJobDataSource where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.LabelingJobS3DataSource

-- | Provides information about the location of input data.
--
--
--
-- /See:/ 'labelingJobDataSource' smart constructor.
newtype LabelingJobDataSource = LabelingJobDataSource'{_ljdsS3DataSource
                                                       ::
                                                       LabelingJobS3DataSource}
                                  deriving (Eq, Read, Show, Data, Typeable,
                                            Generic)

-- | Creates a value of 'LabelingJobDataSource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ljdsS3DataSource' - The Amazon S3 location of the input data objects.
labelingJobDataSource
    :: LabelingJobS3DataSource -- ^ 'ljdsS3DataSource'
    -> LabelingJobDataSource
labelingJobDataSource pS3DataSource_
  = LabelingJobDataSource'{_ljdsS3DataSource =
                             pS3DataSource_}

-- | The Amazon S3 location of the input data objects.
ljdsS3DataSource :: Lens' LabelingJobDataSource LabelingJobS3DataSource
ljdsS3DataSource = lens _ljdsS3DataSource (\ s a -> s{_ljdsS3DataSource = a})

instance FromJSON LabelingJobDataSource where
        parseJSON
          = withObject "LabelingJobDataSource"
              (\ x ->
                 LabelingJobDataSource' <$> (x .: "S3DataSource"))

instance Hashable LabelingJobDataSource where

instance NFData LabelingJobDataSource where

instance ToJSON LabelingJobDataSource where
        toJSON LabelingJobDataSource'{..}
          = object
              (catMaybes
                 [Just ("S3DataSource" .= _ljdsS3DataSource)])
