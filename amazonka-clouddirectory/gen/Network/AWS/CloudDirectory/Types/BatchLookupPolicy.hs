{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchLookupPolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudDirectory.Types.BatchLookupPolicy where

import Network.AWS.CloudDirectory.Types.ObjectReference
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Lists all policies from the root of the Directory to the object specified inside a 'BatchRead' operation. For more information, see 'LookupPolicy' and 'BatchReadRequest$Operations' .
--
--
--
-- /See:/ 'batchLookupPolicy' smart constructor.
data BatchLookupPolicy = BatchLookupPolicy'{_btchlkpplcyNextToken
                                            :: !(Maybe Text),
                                            _btchlkpplcyMaxResults ::
                                            !(Maybe Nat),
                                            _btchlkpplcyObjectReference ::
                                            !ObjectReference}
                           deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchLookupPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'btchlkpplcyNextToken' - The pagination token.
--
-- * 'btchlkpplcyMaxResults' - The maximum number of results to retrieve.
--
-- * 'btchlkpplcyObjectReference' - Reference that identifies the object whose policies will be looked up.
batchLookupPolicy
    :: ObjectReference -- ^ 'btchlkpplcyObjectReference'
    -> BatchLookupPolicy
batchLookupPolicy pObjectReference_
  = BatchLookupPolicy'{_btchlkpplcyNextToken = Nothing,
                       _btchlkpplcyMaxResults = Nothing,
                       _btchlkpplcyObjectReference = pObjectReference_}

-- | The pagination token.
btchlkpplcyNextToken :: Lens' BatchLookupPolicy (Maybe Text)
btchlkpplcyNextToken = lens _btchlkpplcyNextToken (\ s a -> s{_btchlkpplcyNextToken = a})

-- | The maximum number of results to retrieve.
btchlkpplcyMaxResults :: Lens' BatchLookupPolicy (Maybe Natural)
btchlkpplcyMaxResults = lens _btchlkpplcyMaxResults (\ s a -> s{_btchlkpplcyMaxResults = a}) . mapping _Nat

-- | Reference that identifies the object whose policies will be looked up.
btchlkpplcyObjectReference :: Lens' BatchLookupPolicy ObjectReference
btchlkpplcyObjectReference = lens _btchlkpplcyObjectReference (\ s a -> s{_btchlkpplcyObjectReference = a})

instance Hashable BatchLookupPolicy where

instance NFData BatchLookupPolicy where

instance ToJSON BatchLookupPolicy where
        toJSON BatchLookupPolicy'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _btchlkpplcyNextToken,
                  ("MaxResults" .=) <$> _btchlkpplcyMaxResults,
                  Just
                    ("ObjectReference" .= _btchlkpplcyObjectReference)])
