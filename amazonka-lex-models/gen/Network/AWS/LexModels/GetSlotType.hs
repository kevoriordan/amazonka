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
-- Module      : Network.AWS.LexModels.GetSlotType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specific version of a slot type. In addition to specifying the slot type name, you must specify the slot type version.
--
--
-- This operation requires permissions for the @lex:GetSlotType@ action.
--
module Network.AWS.LexModels.GetSlotType
    (
    -- * Creating a Request
      getSlotType
    , GetSlotType
    -- * Request Lenses
    , gstName
    , gstVersion

    -- * Destructuring the Response
    , getSlotTypeResponse
    , GetSlotTypeResponse
    -- * Response Lenses
    , gtslttyprsParentSlotTypeSignature
    , gtslttyprsSlotTypeConfigurations
    , gtslttyprsChecksum
    , gtslttyprsValueSelectionStrategy
    , gtslttyprsCreatedDate
    , gtslttyprsName
    , gtslttyprsVersion
    , gtslttyprsLastUpdatedDate
    , gtslttyprsDescription
    , gtslttyprsEnumerationValues
    , gtslttyprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.LexModels.Types
import Network.AWS.LexModels.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getSlotType' smart constructor.
data GetSlotType = GetSlotType'{_gstName :: !Text,
                                _gstVersion :: !Text}
                     deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetSlotType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gstName' - The name of the slot type. The name is case sensitive. 
--
-- * 'gstVersion' - The version of the slot type. 
getSlotType
    :: Text -- ^ 'gstName'
    -> Text -- ^ 'gstVersion'
    -> GetSlotType
getSlotType pName_ pVersion_
  = GetSlotType'{_gstName = pName_,
                 _gstVersion = pVersion_}

-- | The name of the slot type. The name is case sensitive. 
gstName :: Lens' GetSlotType Text
gstName = lens _gstName (\ s a -> s{_gstName = a})

-- | The version of the slot type. 
gstVersion :: Lens' GetSlotType Text
gstVersion = lens _gstVersion (\ s a -> s{_gstVersion = a})

instance AWSRequest GetSlotType where
        type Rs GetSlotType = GetSlotTypeResponse
        request = get lexModels
        response
          = receiveJSON
              (\ s h x ->
                 GetSlotTypeResponse' <$>
                   (x .?> "parentSlotTypeSignature") <*>
                     (x .?> "slotTypeConfigurations" .!@ mempty)
                     <*> (x .?> "checksum")
                     <*> (x .?> "valueSelectionStrategy")
                     <*> (x .?> "createdDate")
                     <*> (x .?> "name")
                     <*> (x .?> "version")
                     <*> (x .?> "lastUpdatedDate")
                     <*> (x .?> "description")
                     <*> (x .?> "enumerationValues" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable GetSlotType where

instance NFData GetSlotType where

instance ToHeaders GetSlotType where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetSlotType where
        toPath GetSlotType'{..}
          = mconcat
              ["/slottypes/", toBS _gstName, "/versions/",
               toBS _gstVersion]

instance ToQuery GetSlotType where
        toQuery = const mempty

-- | /See:/ 'getSlotTypeResponse' smart constructor.
data GetSlotTypeResponse = GetSlotTypeResponse'{_gtslttyprsParentSlotTypeSignature
                                                :: !(Maybe Text),
                                                _gtslttyprsSlotTypeConfigurations
                                                ::
                                                !(Maybe
                                                    [SlotTypeConfiguration]),
                                                _gtslttyprsChecksum ::
                                                !(Maybe Text),
                                                _gtslttyprsValueSelectionStrategy
                                                ::
                                                !(Maybe
                                                    SlotValueSelectionStrategy),
                                                _gtslttyprsCreatedDate ::
                                                !(Maybe POSIX),
                                                _gtslttyprsName ::
                                                !(Maybe Text),
                                                _gtslttyprsVersion ::
                                                !(Maybe Text),
                                                _gtslttyprsLastUpdatedDate ::
                                                !(Maybe POSIX),
                                                _gtslttyprsDescription ::
                                                !(Maybe Text),
                                                _gtslttyprsEnumerationValues ::
                                                !(Maybe [EnumerationValue]),
                                                _gtslttyprsResponseStatus ::
                                                !Int}
                             deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetSlotTypeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtslttyprsParentSlotTypeSignature' - The built-in slot type used as a parent for the slot type.
--
-- * 'gtslttyprsSlotTypeConfigurations' - Configuration information that extends the parent built-in slot type.
--
-- * 'gtslttyprsChecksum' - Checksum of the @> LATEST@ version of the slot type.
--
-- * 'gtslttyprsValueSelectionStrategy' - The strategy that Amazon Lex uses to determine the value of the slot. For more information, see 'PutSlotType' .
--
-- * 'gtslttyprsCreatedDate' - The date that the slot type was created.
--
-- * 'gtslttyprsName' - The name of the slot type.
--
-- * 'gtslttyprsVersion' - The version of the slot type.
--
-- * 'gtslttyprsLastUpdatedDate' - The date that the slot type was updated. When you create a resource, the creation date and last update date are the same.
--
-- * 'gtslttyprsDescription' - A description of the slot type.
--
-- * 'gtslttyprsEnumerationValues' - A list of @EnumerationValue@ objects that defines the values that the slot type can take.
--
-- * 'gtslttyprsResponseStatus' - -- | The response status code.
getSlotTypeResponse
    :: Int -- ^ 'gtslttyprsResponseStatus'
    -> GetSlotTypeResponse
getSlotTypeResponse pResponseStatus_
  = GetSlotTypeResponse'{_gtslttyprsParentSlotTypeSignature
                           = Nothing,
                         _gtslttyprsSlotTypeConfigurations = Nothing,
                         _gtslttyprsChecksum = Nothing,
                         _gtslttyprsValueSelectionStrategy = Nothing,
                         _gtslttyprsCreatedDate = Nothing,
                         _gtslttyprsName = Nothing,
                         _gtslttyprsVersion = Nothing,
                         _gtslttyprsLastUpdatedDate = Nothing,
                         _gtslttyprsDescription = Nothing,
                         _gtslttyprsEnumerationValues = Nothing,
                         _gtslttyprsResponseStatus = pResponseStatus_}

-- | The built-in slot type used as a parent for the slot type.
gtslttyprsParentSlotTypeSignature :: Lens' GetSlotTypeResponse (Maybe Text)
gtslttyprsParentSlotTypeSignature = lens _gtslttyprsParentSlotTypeSignature (\ s a -> s{_gtslttyprsParentSlotTypeSignature = a})

-- | Configuration information that extends the parent built-in slot type.
gtslttyprsSlotTypeConfigurations :: Lens' GetSlotTypeResponse [SlotTypeConfiguration]
gtslttyprsSlotTypeConfigurations = lens _gtslttyprsSlotTypeConfigurations (\ s a -> s{_gtslttyprsSlotTypeConfigurations = a}) . _Default . _Coerce

-- | Checksum of the @> LATEST@ version of the slot type.
gtslttyprsChecksum :: Lens' GetSlotTypeResponse (Maybe Text)
gtslttyprsChecksum = lens _gtslttyprsChecksum (\ s a -> s{_gtslttyprsChecksum = a})

-- | The strategy that Amazon Lex uses to determine the value of the slot. For more information, see 'PutSlotType' .
gtslttyprsValueSelectionStrategy :: Lens' GetSlotTypeResponse (Maybe SlotValueSelectionStrategy)
gtslttyprsValueSelectionStrategy = lens _gtslttyprsValueSelectionStrategy (\ s a -> s{_gtslttyprsValueSelectionStrategy = a})

-- | The date that the slot type was created.
gtslttyprsCreatedDate :: Lens' GetSlotTypeResponse (Maybe UTCTime)
gtslttyprsCreatedDate = lens _gtslttyprsCreatedDate (\ s a -> s{_gtslttyprsCreatedDate = a}) . mapping _Time

-- | The name of the slot type.
gtslttyprsName :: Lens' GetSlotTypeResponse (Maybe Text)
gtslttyprsName = lens _gtslttyprsName (\ s a -> s{_gtslttyprsName = a})

-- | The version of the slot type.
gtslttyprsVersion :: Lens' GetSlotTypeResponse (Maybe Text)
gtslttyprsVersion = lens _gtslttyprsVersion (\ s a -> s{_gtslttyprsVersion = a})

-- | The date that the slot type was updated. When you create a resource, the creation date and last update date are the same.
gtslttyprsLastUpdatedDate :: Lens' GetSlotTypeResponse (Maybe UTCTime)
gtslttyprsLastUpdatedDate = lens _gtslttyprsLastUpdatedDate (\ s a -> s{_gtslttyprsLastUpdatedDate = a}) . mapping _Time

-- | A description of the slot type.
gtslttyprsDescription :: Lens' GetSlotTypeResponse (Maybe Text)
gtslttyprsDescription = lens _gtslttyprsDescription (\ s a -> s{_gtslttyprsDescription = a})

-- | A list of @EnumerationValue@ objects that defines the values that the slot type can take.
gtslttyprsEnumerationValues :: Lens' GetSlotTypeResponse [EnumerationValue]
gtslttyprsEnumerationValues = lens _gtslttyprsEnumerationValues (\ s a -> s{_gtslttyprsEnumerationValues = a}) . _Default . _Coerce

-- | -- | The response status code.
gtslttyprsResponseStatus :: Lens' GetSlotTypeResponse Int
gtslttyprsResponseStatus = lens _gtslttyprsResponseStatus (\ s a -> s{_gtslttyprsResponseStatus = a})

instance NFData GetSlotTypeResponse where
