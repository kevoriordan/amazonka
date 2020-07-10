{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchListObjectPolicies
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudDirectory.Types.BatchListObjectPolicies where

import Network.AWS.CloudDirectory.Types.ObjectReference
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Returns policies attached to an object in pagination fashion inside a 'BatchRead' operation. For more information, see 'ListObjectPolicies' and 'BatchReadRequest$Operations' .
--
--
--
-- /See:/ 'batchListObjectPolicies' smart constructor.
data BatchListObjectPolicies = BatchListObjectPolicies'{_btchlstobjctplcsNextToken
                                                        :: !(Maybe Text),
                                                        _btchlstobjctplcsMaxResults
                                                        :: !(Maybe Nat),
                                                        _btchlstobjctplcsObjectReference
                                                        :: !ObjectReference}
                                 deriving (Eq, Read, Show, Data, Typeable,
                                           Generic)

-- | Creates a value of 'BatchListObjectPolicies' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'btchlstobjctplcsNextToken' - The pagination token.
--
-- * 'btchlstobjctplcsMaxResults' - The maximum number of results to retrieve.
--
-- * 'btchlstobjctplcsObjectReference' - The reference that identifies the object whose attributes will be listed.
batchListObjectPolicies
    :: ObjectReference -- ^ 'btchlstobjctplcsObjectReference'
    -> BatchListObjectPolicies
batchListObjectPolicies pObjectReference_
  = BatchListObjectPolicies'{_btchlstobjctplcsNextToken
                               = Nothing,
                             _btchlstobjctplcsMaxResults = Nothing,
                             _btchlstobjctplcsObjectReference =
                               pObjectReference_}

-- | The pagination token.
btchlstobjctplcsNextToken :: Lens' BatchListObjectPolicies (Maybe Text)
btchlstobjctplcsNextToken = lens _btchlstobjctplcsNextToken (\ s a -> s{_btchlstobjctplcsNextToken = a})

-- | The maximum number of results to retrieve.
btchlstobjctplcsMaxResults :: Lens' BatchListObjectPolicies (Maybe Natural)
btchlstobjctplcsMaxResults = lens _btchlstobjctplcsMaxResults (\ s a -> s{_btchlstobjctplcsMaxResults = a}) . mapping _Nat

-- | The reference that identifies the object whose attributes will be listed.
btchlstobjctplcsObjectReference :: Lens' BatchListObjectPolicies ObjectReference
btchlstobjctplcsObjectReference = lens _btchlstobjctplcsObjectReference (\ s a -> s{_btchlstobjctplcsObjectReference = a})

instance Hashable BatchListObjectPolicies where

instance NFData BatchListObjectPolicies where

instance ToJSON BatchListObjectPolicies where
        toJSON BatchListObjectPolicies'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _btchlstobjctplcsNextToken,
                  ("MaxResults" .=) <$> _btchlstobjctplcsMaxResults,
                  Just
                    ("ObjectReference" .=
                       _btchlstobjctplcsObjectReference)])
