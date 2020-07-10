{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.Operation
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.Operation where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types.OperationStatus
import Network.AWS.Lightsail.Types.OperationType
import Network.AWS.Lightsail.Types.ResourceLocation
import Network.AWS.Lightsail.Types.ResourceType
import Network.AWS.Prelude

-- | Describes the API operation.
--
--
--
-- /See:/ 'operation' smart constructor.
data Operation = Operation'{_oStatus ::
                            !(Maybe OperationStatus),
                            _oOperationDetails :: !(Maybe Text),
                            _oResourceType :: !(Maybe ResourceType),
                            _oCreatedAt :: !(Maybe POSIX),
                            _oResourceName :: !(Maybe Text),
                            _oLocation :: !(Maybe ResourceLocation),
                            _oStatusChangedAt :: !(Maybe POSIX),
                            _oErrorDetails :: !(Maybe Text),
                            _oErrorCode :: !(Maybe Text), _oId :: !(Maybe Text),
                            _oOperationType :: !(Maybe OperationType),
                            _oIsTerminal :: !(Maybe Bool)}
                   deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Operation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oStatus' - The status of the operation. 
--
-- * 'oOperationDetails' - Details about the operation (e.g., @Debian-1GB-Ohio-1@ ).
--
-- * 'oResourceType' - The resource type. 
--
-- * 'oCreatedAt' - The timestamp when the operation was initialized (e.g., @1479816991.349@ ).
--
-- * 'oResourceName' - The resource name.
--
-- * 'oLocation' - The AWS Region and Availability Zone.
--
-- * 'oStatusChangedAt' - The timestamp when the status was changed (e.g., @1479816991.349@ ).
--
-- * 'oErrorDetails' - The error details.
--
-- * 'oErrorCode' - The error code.
--
-- * 'oId' - The ID of the operation.
--
-- * 'oOperationType' - The type of operation. 
--
-- * 'oIsTerminal' - A Boolean value indicating whether the operation is terminal.
operation
    :: Operation
operation
  = Operation'{_oStatus = Nothing,
               _oOperationDetails = Nothing,
               _oResourceType = Nothing, _oCreatedAt = Nothing,
               _oResourceName = Nothing, _oLocation = Nothing,
               _oStatusChangedAt = Nothing,
               _oErrorDetails = Nothing, _oErrorCode = Nothing,
               _oId = Nothing, _oOperationType = Nothing,
               _oIsTerminal = Nothing}

-- | The status of the operation. 
oStatus :: Lens' Operation (Maybe OperationStatus)
oStatus = lens _oStatus (\ s a -> s{_oStatus = a})

-- | Details about the operation (e.g., @Debian-1GB-Ohio-1@ ).
oOperationDetails :: Lens' Operation (Maybe Text)
oOperationDetails = lens _oOperationDetails (\ s a -> s{_oOperationDetails = a})

-- | The resource type. 
oResourceType :: Lens' Operation (Maybe ResourceType)
oResourceType = lens _oResourceType (\ s a -> s{_oResourceType = a})

-- | The timestamp when the operation was initialized (e.g., @1479816991.349@ ).
oCreatedAt :: Lens' Operation (Maybe UTCTime)
oCreatedAt = lens _oCreatedAt (\ s a -> s{_oCreatedAt = a}) . mapping _Time

-- | The resource name.
oResourceName :: Lens' Operation (Maybe Text)
oResourceName = lens _oResourceName (\ s a -> s{_oResourceName = a})

-- | The AWS Region and Availability Zone.
oLocation :: Lens' Operation (Maybe ResourceLocation)
oLocation = lens _oLocation (\ s a -> s{_oLocation = a})

-- | The timestamp when the status was changed (e.g., @1479816991.349@ ).
oStatusChangedAt :: Lens' Operation (Maybe UTCTime)
oStatusChangedAt = lens _oStatusChangedAt (\ s a -> s{_oStatusChangedAt = a}) . mapping _Time

-- | The error details.
oErrorDetails :: Lens' Operation (Maybe Text)
oErrorDetails = lens _oErrorDetails (\ s a -> s{_oErrorDetails = a})

-- | The error code.
oErrorCode :: Lens' Operation (Maybe Text)
oErrorCode = lens _oErrorCode (\ s a -> s{_oErrorCode = a})

-- | The ID of the operation.
oId :: Lens' Operation (Maybe Text)
oId = lens _oId (\ s a -> s{_oId = a})

-- | The type of operation. 
oOperationType :: Lens' Operation (Maybe OperationType)
oOperationType = lens _oOperationType (\ s a -> s{_oOperationType = a})

-- | A Boolean value indicating whether the operation is terminal.
oIsTerminal :: Lens' Operation (Maybe Bool)
oIsTerminal = lens _oIsTerminal (\ s a -> s{_oIsTerminal = a})

instance FromJSON Operation where
        parseJSON
          = withObject "Operation"
              (\ x ->
                 Operation' <$>
                   (x .:? "status") <*> (x .:? "operationDetails") <*>
                     (x .:? "resourceType")
                     <*> (x .:? "createdAt")
                     <*> (x .:? "resourceName")
                     <*> (x .:? "location")
                     <*> (x .:? "statusChangedAt")
                     <*> (x .:? "errorDetails")
                     <*> (x .:? "errorCode")
                     <*> (x .:? "id")
                     <*> (x .:? "operationType")
                     <*> (x .:? "isTerminal"))

instance Hashable Operation where

instance NFData Operation where
