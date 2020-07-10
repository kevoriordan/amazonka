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
-- Module      : Network.AWS.CloudSearch.DeleteSuggester
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a suggester. For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/getting-suggestions.html Getting Search Suggestions> in the /Amazon CloudSearch Developer Guide/ .
--
--
module Network.AWS.CloudSearch.DeleteSuggester
    (
    -- * Creating a Request
      deleteSuggester
    , DeleteSuggester
    -- * Request Lenses
    , dltsggstrDomainName
    , dltsggstrSuggesterName

    -- * Destructuring the Response
    , deleteSuggesterResponse
    , DeleteSuggesterResponse
    -- * Response Lenses
    , dltsggstrrsResponseStatus
    , dltsggstrrsSuggester
    ) where

import Network.AWS.CloudSearch.Types
import Network.AWS.CloudSearch.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Container for the parameters to the @'DeleteSuggester' @ operation. Specifies the name of the domain you want to update and name of the suggester you want to delete.
--
--
--
-- /See:/ 'deleteSuggester' smart constructor.
data DeleteSuggester = DeleteSuggester'{_dltsggstrDomainName
                                        :: !Text,
                                        _dltsggstrSuggesterName :: !Text}
                         deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteSuggester' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dltsggstrDomainName' - Undocumented member.
--
-- * 'dltsggstrSuggesterName' - Specifies the name of the suggester you want to delete.
deleteSuggester
    :: Text -- ^ 'dltsggstrDomainName'
    -> Text -- ^ 'dltsggstrSuggesterName'
    -> DeleteSuggester
deleteSuggester pDomainName_ pSuggesterName_
  = DeleteSuggester'{_dltsggstrDomainName =
                       pDomainName_,
                     _dltsggstrSuggesterName = pSuggesterName_}

-- | Undocumented member.
dltsggstrDomainName :: Lens' DeleteSuggester Text
dltsggstrDomainName = lens _dltsggstrDomainName (\ s a -> s{_dltsggstrDomainName = a})

-- | Specifies the name of the suggester you want to delete.
dltsggstrSuggesterName :: Lens' DeleteSuggester Text
dltsggstrSuggesterName = lens _dltsggstrSuggesterName (\ s a -> s{_dltsggstrSuggesterName = a})

instance AWSRequest DeleteSuggester where
        type Rs DeleteSuggester = DeleteSuggesterResponse
        request = postQuery cloudSearch
        response
          = receiveXMLWrapper "DeleteSuggesterResult"
              (\ s h x ->
                 DeleteSuggesterResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "Suggester"))

instance Hashable DeleteSuggester where

instance NFData DeleteSuggester where

instance ToHeaders DeleteSuggester where
        toHeaders = const mempty

instance ToPath DeleteSuggester where
        toPath = const "/"

instance ToQuery DeleteSuggester where
        toQuery DeleteSuggester'{..}
          = mconcat
              ["Action" =: ("DeleteSuggester" :: ByteString),
               "Version" =: ("2013-01-01" :: ByteString),
               "DomainName" =: _dltsggstrDomainName,
               "SuggesterName" =: _dltsggstrSuggesterName]

-- | The result of a @DeleteSuggester@ request. Contains the status of the deleted suggester.
--
--
--
-- /See:/ 'deleteSuggesterResponse' smart constructor.
data DeleteSuggesterResponse = DeleteSuggesterResponse'{_dltsggstrrsResponseStatus
                                                        :: !Int,
                                                        _dltsggstrrsSuggester ::
                                                        !SuggesterStatus}
                                 deriving (Eq, Read, Show, Data, Typeable,
                                           Generic)

-- | Creates a value of 'DeleteSuggesterResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dltsggstrrsResponseStatus' - -- | The response status code.
--
-- * 'dltsggstrrsSuggester' - The status of the suggester being deleted.
deleteSuggesterResponse
    :: Int -- ^ 'dltsggstrrsResponseStatus'
    -> SuggesterStatus -- ^ 'dltsggstrrsSuggester'
    -> DeleteSuggesterResponse
deleteSuggesterResponse pResponseStatus_ pSuggester_
  = DeleteSuggesterResponse'{_dltsggstrrsResponseStatus
                               = pResponseStatus_,
                             _dltsggstrrsSuggester = pSuggester_}

-- | -- | The response status code.
dltsggstrrsResponseStatus :: Lens' DeleteSuggesterResponse Int
dltsggstrrsResponseStatus = lens _dltsggstrrsResponseStatus (\ s a -> s{_dltsggstrrsResponseStatus = a})

-- | The status of the suggester being deleted.
dltsggstrrsSuggester :: Lens' DeleteSuggesterResponse SuggesterStatus
dltsggstrrsSuggester = lens _dltsggstrrsSuggester (\ s a -> s{_dltsggstrrsSuggester = a})

instance NFData DeleteSuggesterResponse where
