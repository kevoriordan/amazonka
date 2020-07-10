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
-- Module      : Network.AWS.CloudSearch.DefineAnalysisScheme
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Configures an analysis scheme that can be applied to a @text@ or @text-array@ field to define language-specific text processing options. For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-analysis-schemes.html Configuring Analysis Schemes> in the /Amazon CloudSearch Developer Guide/ .
--
--
module Network.AWS.CloudSearch.DefineAnalysisScheme
    (
    -- * Creating a Request
      defineAnalysisScheme
    , DefineAnalysisScheme
    -- * Request Lenses
    , dfnanlyssschmDomainName
    , dfnanlyssschmAnalysisScheme

    -- * Destructuring the Response
    , defineAnalysisSchemeResponse
    , DefineAnalysisSchemeResponse
    -- * Response Lenses
    , dfnanlyssschmrsResponseStatus
    , dfnanlyssschmrsAnalysisScheme
    ) where

import Network.AWS.CloudSearch.Types
import Network.AWS.CloudSearch.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Container for the parameters to the @'DefineAnalysisScheme' @ operation. Specifies the name of the domain you want to update and the analysis scheme configuration.
--
--
--
-- /See:/ 'defineAnalysisScheme' smart constructor.
data DefineAnalysisScheme = DefineAnalysisScheme'{_dfnanlyssschmDomainName
                                                  :: !Text,
                                                  _dfnanlyssschmAnalysisScheme
                                                  :: !AnalysisScheme}
                              deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DefineAnalysisScheme' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfnanlyssschmDomainName' - Undocumented member.
--
-- * 'dfnanlyssschmAnalysisScheme' - Undocumented member.
defineAnalysisScheme
    :: Text -- ^ 'dfnanlyssschmDomainName'
    -> AnalysisScheme -- ^ 'dfnanlyssschmAnalysisScheme'
    -> DefineAnalysisScheme
defineAnalysisScheme pDomainName_ pAnalysisScheme_
  = DefineAnalysisScheme'{_dfnanlyssschmDomainName =
                            pDomainName_,
                          _dfnanlyssschmAnalysisScheme = pAnalysisScheme_}

-- | Undocumented member.
dfnanlyssschmDomainName :: Lens' DefineAnalysisScheme Text
dfnanlyssschmDomainName = lens _dfnanlyssschmDomainName (\ s a -> s{_dfnanlyssschmDomainName = a})

-- | Undocumented member.
dfnanlyssschmAnalysisScheme :: Lens' DefineAnalysisScheme AnalysisScheme
dfnanlyssschmAnalysisScheme = lens _dfnanlyssschmAnalysisScheme (\ s a -> s{_dfnanlyssschmAnalysisScheme = a})

instance AWSRequest DefineAnalysisScheme where
        type Rs DefineAnalysisScheme =
             DefineAnalysisSchemeResponse
        request = postQuery cloudSearch
        response
          = receiveXMLWrapper "DefineAnalysisSchemeResult"
              (\ s h x ->
                 DefineAnalysisSchemeResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "AnalysisScheme"))

instance Hashable DefineAnalysisScheme where

instance NFData DefineAnalysisScheme where

instance ToHeaders DefineAnalysisScheme where
        toHeaders = const mempty

instance ToPath DefineAnalysisScheme where
        toPath = const "/"

instance ToQuery DefineAnalysisScheme where
        toQuery DefineAnalysisScheme'{..}
          = mconcat
              ["Action" =: ("DefineAnalysisScheme" :: ByteString),
               "Version" =: ("2013-01-01" :: ByteString),
               "DomainName" =: _dfnanlyssschmDomainName,
               "AnalysisScheme" =: _dfnanlyssschmAnalysisScheme]

-- | The result of a @'DefineAnalysisScheme' @ request. Contains the status of the newly-configured analysis scheme.
--
--
--
-- /See:/ 'defineAnalysisSchemeResponse' smart constructor.
data DefineAnalysisSchemeResponse = DefineAnalysisSchemeResponse'{_dfnanlyssschmrsResponseStatus
                                                                  :: !Int,
                                                                  _dfnanlyssschmrsAnalysisScheme
                                                                  ::
                                                                  !AnalysisSchemeStatus}
                                      deriving (Eq, Read, Show, Data, Typeable,
                                                Generic)

-- | Creates a value of 'DefineAnalysisSchemeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfnanlyssschmrsResponseStatus' - -- | The response status code.
--
-- * 'dfnanlyssschmrsAnalysisScheme' - Undocumented member.
defineAnalysisSchemeResponse
    :: Int -- ^ 'dfnanlyssschmrsResponseStatus'
    -> AnalysisSchemeStatus -- ^ 'dfnanlyssschmrsAnalysisScheme'
    -> DefineAnalysisSchemeResponse
defineAnalysisSchemeResponse pResponseStatus_
  pAnalysisScheme_
  = DefineAnalysisSchemeResponse'{_dfnanlyssschmrsResponseStatus
                                    = pResponseStatus_,
                                  _dfnanlyssschmrsAnalysisScheme =
                                    pAnalysisScheme_}

-- | -- | The response status code.
dfnanlyssschmrsResponseStatus :: Lens' DefineAnalysisSchemeResponse Int
dfnanlyssschmrsResponseStatus = lens _dfnanlyssschmrsResponseStatus (\ s a -> s{_dfnanlyssschmrsResponseStatus = a})

-- | Undocumented member.
dfnanlyssschmrsAnalysisScheme :: Lens' DefineAnalysisSchemeResponse AnalysisSchemeStatus
dfnanlyssschmrsAnalysisScheme = lens _dfnanlyssschmrsAnalysisScheme (\ s a -> s{_dfnanlyssschmrsAnalysisScheme = a})

instance NFData DefineAnalysisSchemeResponse where
