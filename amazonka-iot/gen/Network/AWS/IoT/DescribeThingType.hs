{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DescribeThingType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the specified thing type.
--
--
module Network.AWS.IoT.DescribeThingType
    (
    -- * Creating a Request
      describeThingType
    , DescribeThingType
    -- * Request Lenses
    , dThingTypeName

    -- * Destructuring the Response
    , describeThingTypeResponse
    , DescribeThingTypeResponse
    -- * Response Lenses
    , dscrbthngtyprsThingTypeProperties
    , dscrbthngtyprsThingTypeName
    , dscrbthngtyprsThingTypeId
    , dscrbthngtyprsThingTypeMetadata
    , dscrbthngtyprsThingTypeARN
    , dscrbthngtyprsResponseStatus
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input for the DescribeThingType operation.
--
--
--
-- /See:/ 'describeThingType' smart constructor.
newtype DescribeThingType = DescribeThingType'{_dThingTypeName
                                               :: Text}
                              deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeThingType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dThingTypeName' - The name of the thing type.
describeThingType
    :: Text -- ^ 'dThingTypeName'
    -> DescribeThingType
describeThingType pThingTypeName_
  = DescribeThingType'{_dThingTypeName =
                         pThingTypeName_}

-- | The name of the thing type.
dThingTypeName :: Lens' DescribeThingType Text
dThingTypeName = lens _dThingTypeName (\ s a -> s{_dThingTypeName = a})

instance AWSRequest DescribeThingType where
        type Rs DescribeThingType = DescribeThingTypeResponse
        request = get ioT
        response
          = receiveJSON
              (\ s h x ->
                 DescribeThingTypeResponse' <$>
                   (x .?> "thingTypeProperties") <*>
                     (x .?> "thingTypeName")
                     <*> (x .?> "thingTypeId")
                     <*> (x .?> "thingTypeMetadata")
                     <*> (x .?> "thingTypeArn")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeThingType where

instance NFData DescribeThingType where

instance ToHeaders DescribeThingType where
        toHeaders = const mempty

instance ToPath DescribeThingType where
        toPath DescribeThingType'{..}
          = mconcat ["/thing-types/", toBS _dThingTypeName]

instance ToQuery DescribeThingType where
        toQuery = const mempty

-- | The output for the DescribeThingType operation.
--
--
--
-- /See:/ 'describeThingTypeResponse' smart constructor.
data DescribeThingTypeResponse = DescribeThingTypeResponse'{_dscrbthngtyprsThingTypeProperties
                                                            ::
                                                            !(Maybe
                                                                ThingTypeProperties),
                                                            _dscrbthngtyprsThingTypeName
                                                            :: !(Maybe Text),
                                                            _dscrbthngtyprsThingTypeId
                                                            :: !(Maybe Text),
                                                            _dscrbthngtyprsThingTypeMetadata
                                                            ::
                                                            !(Maybe
                                                                ThingTypeMetadata),
                                                            _dscrbthngtyprsThingTypeARN
                                                            :: !(Maybe Text),
                                                            _dscrbthngtyprsResponseStatus
                                                            :: !Int}
                                   deriving (Eq, Read, Show, Data, Typeable,
                                             Generic)

-- | Creates a value of 'DescribeThingTypeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dscrbthngtyprsThingTypeProperties' - The ThingTypeProperties contains information about the thing type including description, and a list of searchable thing attribute names.
--
-- * 'dscrbthngtyprsThingTypeName' - The name of the thing type.
--
-- * 'dscrbthngtyprsThingTypeId' - The thing type ID.
--
-- * 'dscrbthngtyprsThingTypeMetadata' - The ThingTypeMetadata contains additional information about the thing type including: creation date and time, a value indicating whether the thing type is deprecated, and a date and time when it was deprecated.
--
-- * 'dscrbthngtyprsThingTypeARN' - The thing type ARN.
--
-- * 'dscrbthngtyprsResponseStatus' - -- | The response status code.
describeThingTypeResponse
    :: Int -- ^ 'dscrbthngtyprsResponseStatus'
    -> DescribeThingTypeResponse
describeThingTypeResponse pResponseStatus_
  = DescribeThingTypeResponse'{_dscrbthngtyprsThingTypeProperties
                                 = Nothing,
                               _dscrbthngtyprsThingTypeName = Nothing,
                               _dscrbthngtyprsThingTypeId = Nothing,
                               _dscrbthngtyprsThingTypeMetadata = Nothing,
                               _dscrbthngtyprsThingTypeARN = Nothing,
                               _dscrbthngtyprsResponseStatus = pResponseStatus_}

-- | The ThingTypeProperties contains information about the thing type including description, and a list of searchable thing attribute names.
dscrbthngtyprsThingTypeProperties :: Lens' DescribeThingTypeResponse (Maybe ThingTypeProperties)
dscrbthngtyprsThingTypeProperties = lens _dscrbthngtyprsThingTypeProperties (\ s a -> s{_dscrbthngtyprsThingTypeProperties = a})

-- | The name of the thing type.
dscrbthngtyprsThingTypeName :: Lens' DescribeThingTypeResponse (Maybe Text)
dscrbthngtyprsThingTypeName = lens _dscrbthngtyprsThingTypeName (\ s a -> s{_dscrbthngtyprsThingTypeName = a})

-- | The thing type ID.
dscrbthngtyprsThingTypeId :: Lens' DescribeThingTypeResponse (Maybe Text)
dscrbthngtyprsThingTypeId = lens _dscrbthngtyprsThingTypeId (\ s a -> s{_dscrbthngtyprsThingTypeId = a})

-- | The ThingTypeMetadata contains additional information about the thing type including: creation date and time, a value indicating whether the thing type is deprecated, and a date and time when it was deprecated.
dscrbthngtyprsThingTypeMetadata :: Lens' DescribeThingTypeResponse (Maybe ThingTypeMetadata)
dscrbthngtyprsThingTypeMetadata = lens _dscrbthngtyprsThingTypeMetadata (\ s a -> s{_dscrbthngtyprsThingTypeMetadata = a})

-- | The thing type ARN.
dscrbthngtyprsThingTypeARN :: Lens' DescribeThingTypeResponse (Maybe Text)
dscrbthngtyprsThingTypeARN = lens _dscrbthngtyprsThingTypeARN (\ s a -> s{_dscrbthngtyprsThingTypeARN = a})

-- | -- | The response status code.
dscrbthngtyprsResponseStatus :: Lens' DescribeThingTypeResponse Int
dscrbthngtyprsResponseStatus = lens _dscrbthngtyprsResponseStatus (\ s a -> s{_dscrbthngtyprsResponseStatus = a})

instance NFData DescribeThingTypeResponse where
