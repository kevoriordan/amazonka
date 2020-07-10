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
-- Module      : Network.AWS.IoT.DescribeDimension
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides details about a dimension that is defined in your AWS account.
--
--
module Network.AWS.IoT.DescribeDimension
    (
    -- * Creating a Request
      describeDimension
    , DescribeDimension
    -- * Request Lenses
    , ddName

    -- * Destructuring the Response
    , describeDimensionResponse
    , DescribeDimensionResponse
    -- * Response Lenses
    , dscrbdmnsnrsLastModifiedDate
    , dscrbdmnsnrsArn
    , dscrbdmnsnrsStringValues
    , dscrbdmnsnrsName
    , dscrbdmnsnrsCreationDate
    , dscrbdmnsnrsType
    , dscrbdmnsnrsResponseStatus
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeDimension' smart constructor.
newtype DescribeDimension = DescribeDimension'{_ddName
                                               :: Text}
                              deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeDimension' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddName' - The unique identifier for the dimension.
describeDimension
    :: Text -- ^ 'ddName'
    -> DescribeDimension
describeDimension pName_
  = DescribeDimension'{_ddName = pName_}

-- | The unique identifier for the dimension.
ddName :: Lens' DescribeDimension Text
ddName = lens _ddName (\ s a -> s{_ddName = a})

instance AWSRequest DescribeDimension where
        type Rs DescribeDimension = DescribeDimensionResponse
        request = get ioT
        response
          = receiveJSON
              (\ s h x ->
                 DescribeDimensionResponse' <$>
                   (x .?> "lastModifiedDate") <*> (x .?> "arn") <*>
                     (x .?> "stringValues")
                     <*> (x .?> "name")
                     <*> (x .?> "creationDate")
                     <*> (x .?> "type")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeDimension where

instance NFData DescribeDimension where

instance ToHeaders DescribeDimension where
        toHeaders = const mempty

instance ToPath DescribeDimension where
        toPath DescribeDimension'{..}
          = mconcat ["/dimensions/", toBS _ddName]

instance ToQuery DescribeDimension where
        toQuery = const mempty

-- | /See:/ 'describeDimensionResponse' smart constructor.
data DescribeDimensionResponse = DescribeDimensionResponse'{_dscrbdmnsnrsLastModifiedDate
                                                            :: !(Maybe POSIX),
                                                            _dscrbdmnsnrsArn ::
                                                            !(Maybe Text),
                                                            _dscrbdmnsnrsStringValues
                                                            ::
                                                            !(Maybe
                                                                (List1 Text)),
                                                            _dscrbdmnsnrsName ::
                                                            !(Maybe Text),
                                                            _dscrbdmnsnrsCreationDate
                                                            :: !(Maybe POSIX),
                                                            _dscrbdmnsnrsType ::
                                                            !(Maybe
                                                                DimensionType),
                                                            _dscrbdmnsnrsResponseStatus
                                                            :: !Int}
                                   deriving (Eq, Read, Show, Data, Typeable,
                                             Generic)

-- | Creates a value of 'DescribeDimensionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dscrbdmnsnrsLastModifiedDate' - The date the dimension was last modified.
--
-- * 'dscrbdmnsnrsArn' - The ARN (Amazon resource name) for the dimension.
--
-- * 'dscrbdmnsnrsStringValues' - The value or list of values used to scope the dimension. For example, for topic filters, this is the pattern used to match the MQTT topic name.
--
-- * 'dscrbdmnsnrsName' - The unique identifier for the dimension.
--
-- * 'dscrbdmnsnrsCreationDate' - The date the dimension was created.
--
-- * 'dscrbdmnsnrsType' - The type of the dimension.
--
-- * 'dscrbdmnsnrsResponseStatus' - -- | The response status code.
describeDimensionResponse
    :: Int -- ^ 'dscrbdmnsnrsResponseStatus'
    -> DescribeDimensionResponse
describeDimensionResponse pResponseStatus_
  = DescribeDimensionResponse'{_dscrbdmnsnrsLastModifiedDate
                                 = Nothing,
                               _dscrbdmnsnrsArn = Nothing,
                               _dscrbdmnsnrsStringValues = Nothing,
                               _dscrbdmnsnrsName = Nothing,
                               _dscrbdmnsnrsCreationDate = Nothing,
                               _dscrbdmnsnrsType = Nothing,
                               _dscrbdmnsnrsResponseStatus = pResponseStatus_}

-- | The date the dimension was last modified.
dscrbdmnsnrsLastModifiedDate :: Lens' DescribeDimensionResponse (Maybe UTCTime)
dscrbdmnsnrsLastModifiedDate = lens _dscrbdmnsnrsLastModifiedDate (\ s a -> s{_dscrbdmnsnrsLastModifiedDate = a}) . mapping _Time

-- | The ARN (Amazon resource name) for the dimension.
dscrbdmnsnrsArn :: Lens' DescribeDimensionResponse (Maybe Text)
dscrbdmnsnrsArn = lens _dscrbdmnsnrsArn (\ s a -> s{_dscrbdmnsnrsArn = a})

-- | The value or list of values used to scope the dimension. For example, for topic filters, this is the pattern used to match the MQTT topic name.
dscrbdmnsnrsStringValues :: Lens' DescribeDimensionResponse (Maybe (NonEmpty Text))
dscrbdmnsnrsStringValues = lens _dscrbdmnsnrsStringValues (\ s a -> s{_dscrbdmnsnrsStringValues = a}) . mapping _List1

-- | The unique identifier for the dimension.
dscrbdmnsnrsName :: Lens' DescribeDimensionResponse (Maybe Text)
dscrbdmnsnrsName = lens _dscrbdmnsnrsName (\ s a -> s{_dscrbdmnsnrsName = a})

-- | The date the dimension was created.
dscrbdmnsnrsCreationDate :: Lens' DescribeDimensionResponse (Maybe UTCTime)
dscrbdmnsnrsCreationDate = lens _dscrbdmnsnrsCreationDate (\ s a -> s{_dscrbdmnsnrsCreationDate = a}) . mapping _Time

-- | The type of the dimension.
dscrbdmnsnrsType :: Lens' DescribeDimensionResponse (Maybe DimensionType)
dscrbdmnsnrsType = lens _dscrbdmnsnrsType (\ s a -> s{_dscrbdmnsnrsType = a})

-- | -- | The response status code.
dscrbdmnsnrsResponseStatus :: Lens' DescribeDimensionResponse Int
dscrbdmnsnrsResponseStatus = lens _dscrbdmnsnrsResponseStatus (\ s a -> s{_dscrbdmnsnrsResponseStatus = a})

instance NFData DescribeDimensionResponse where
