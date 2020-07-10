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
-- Module      : Network.AWS.SSM.DeleteDocument
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the Systems Manager document and all instance associations to the document.
--
--
-- Before you delete the document, we recommend that you use 'DeleteAssociation' to disassociate all instances that are associated with the document.
--
module Network.AWS.SSM.DeleteDocument
    (
    -- * Creating a Request
      deleteDocument
    , DeleteDocument
    -- * Request Lenses
    , dltdcmntVersionName
    , dltdcmntForce
    , dltdcmntDocumentVersion
    , dltdcmntName

    -- * Destructuring the Response
    , deleteDocumentResponse
    , DeleteDocumentResponse
    -- * Response Lenses
    , ddrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'deleteDocument' smart constructor.
data DeleteDocument = DeleteDocument'{_dltdcmntVersionName
                                      :: !(Maybe Text),
                                      _dltdcmntForce :: !(Maybe Bool),
                                      _dltdcmntDocumentVersion :: !(Maybe Text),
                                      _dltdcmntName :: !Text}
                        deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteDocument' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dltdcmntVersionName' - The version name of the document that you want to delete. If not provided, all versions of the document are deleted.
--
-- * 'dltdcmntForce' - Some SSM document types require that you specify a @Force@ flag before you can delete the document. For example, you must specify a @Force@ flag to delete a document of type @ApplicationConfigurationSchema@ . You can restrict access to the @Force@ flag in an AWS Identity and Access Management (IAM) policy.
--
-- * 'dltdcmntDocumentVersion' - The version of the document that you want to delete. If not provided, all versions of the document are deleted.
--
-- * 'dltdcmntName' - The name of the document.
deleteDocument
    :: Text -- ^ 'dltdcmntName'
    -> DeleteDocument
deleteDocument pName_
  = DeleteDocument'{_dltdcmntVersionName = Nothing,
                    _dltdcmntForce = Nothing,
                    _dltdcmntDocumentVersion = Nothing,
                    _dltdcmntName = pName_}

-- | The version name of the document that you want to delete. If not provided, all versions of the document are deleted.
dltdcmntVersionName :: Lens' DeleteDocument (Maybe Text)
dltdcmntVersionName = lens _dltdcmntVersionName (\ s a -> s{_dltdcmntVersionName = a})

-- | Some SSM document types require that you specify a @Force@ flag before you can delete the document. For example, you must specify a @Force@ flag to delete a document of type @ApplicationConfigurationSchema@ . You can restrict access to the @Force@ flag in an AWS Identity and Access Management (IAM) policy.
dltdcmntForce :: Lens' DeleteDocument (Maybe Bool)
dltdcmntForce = lens _dltdcmntForce (\ s a -> s{_dltdcmntForce = a})

-- | The version of the document that you want to delete. If not provided, all versions of the document are deleted.
dltdcmntDocumentVersion :: Lens' DeleteDocument (Maybe Text)
dltdcmntDocumentVersion = lens _dltdcmntDocumentVersion (\ s a -> s{_dltdcmntDocumentVersion = a})

-- | The name of the document.
dltdcmntName :: Lens' DeleteDocument Text
dltdcmntName = lens _dltdcmntName (\ s a -> s{_dltdcmntName = a})

instance AWSRequest DeleteDocument where
        type Rs DeleteDocument = DeleteDocumentResponse
        request = postJSON ssm
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteDocumentResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteDocument where

instance NFData DeleteDocument where

instance ToHeaders DeleteDocument where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.DeleteDocument" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteDocument where
        toJSON DeleteDocument'{..}
          = object
              (catMaybes
                 [("VersionName" .=) <$> _dltdcmntVersionName,
                  ("Force" .=) <$> _dltdcmntForce,
                  ("DocumentVersion" .=) <$> _dltdcmntDocumentVersion,
                  Just ("Name" .= _dltdcmntName)])

instance ToPath DeleteDocument where
        toPath = const "/"

instance ToQuery DeleteDocument where
        toQuery = const mempty

-- | /See:/ 'deleteDocumentResponse' smart constructor.
newtype DeleteDocumentResponse = DeleteDocumentResponse'{_ddrsResponseStatus
                                                         :: Int}
                                   deriving (Eq, Read, Show, Data, Typeable,
                                             Generic)

-- | Creates a value of 'DeleteDocumentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddrsResponseStatus' - -- | The response status code.
deleteDocumentResponse
    :: Int -- ^ 'ddrsResponseStatus'
    -> DeleteDocumentResponse
deleteDocumentResponse pResponseStatus_
  = DeleteDocumentResponse'{_ddrsResponseStatus =
                              pResponseStatus_}

-- | -- | The response status code.
ddrsResponseStatus :: Lens' DeleteDocumentResponse Int
ddrsResponseStatus = lens _ddrsResponseStatus (\ s a -> s{_ddrsResponseStatus = a})

instance NFData DeleteDocumentResponse where
