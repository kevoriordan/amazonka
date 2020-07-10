{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchListObjectChildren
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudDirectory.Types.BatchListObjectChildren where

import Network.AWS.CloudDirectory.Types.ObjectReference
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the output of a 'ListObjectChildren' operation.
--
--
--
-- /See:/ 'batchListObjectChildren' smart constructor.
data BatchListObjectChildren = BatchListObjectChildren'{_btchlstobjctchldrnNextToken
                                                        :: !(Maybe Text),
                                                        _btchlstobjctchldrnMaxResults
                                                        :: !(Maybe Nat),
                                                        _btchlstobjctchldrnObjectReference
                                                        :: !ObjectReference}
                                 deriving (Eq, Read, Show, Data, Typeable,
                                           Generic)

-- | Creates a value of 'BatchListObjectChildren' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'btchlstobjctchldrnNextToken' - The pagination token.
--
-- * 'btchlstobjctchldrnMaxResults' - Maximum number of items to be retrieved in a single call. This is an approximate number.
--
-- * 'btchlstobjctchldrnObjectReference' - Reference of the object for which child objects are being listed.
batchListObjectChildren
    :: ObjectReference -- ^ 'btchlstobjctchldrnObjectReference'
    -> BatchListObjectChildren
batchListObjectChildren pObjectReference_
  = BatchListObjectChildren'{_btchlstobjctchldrnNextToken
                               = Nothing,
                             _btchlstobjctchldrnMaxResults = Nothing,
                             _btchlstobjctchldrnObjectReference =
                               pObjectReference_}

-- | The pagination token.
btchlstobjctchldrnNextToken :: Lens' BatchListObjectChildren (Maybe Text)
btchlstobjctchldrnNextToken = lens _btchlstobjctchldrnNextToken (\ s a -> s{_btchlstobjctchldrnNextToken = a})

-- | Maximum number of items to be retrieved in a single call. This is an approximate number.
btchlstobjctchldrnMaxResults :: Lens' BatchListObjectChildren (Maybe Natural)
btchlstobjctchldrnMaxResults = lens _btchlstobjctchldrnMaxResults (\ s a -> s{_btchlstobjctchldrnMaxResults = a}) . mapping _Nat

-- | Reference of the object for which child objects are being listed.
btchlstobjctchldrnObjectReference :: Lens' BatchListObjectChildren ObjectReference
btchlstobjctchldrnObjectReference = lens _btchlstobjctchldrnObjectReference (\ s a -> s{_btchlstobjctchldrnObjectReference = a})

instance Hashable BatchListObjectChildren where

instance NFData BatchListObjectChildren where

instance ToJSON BatchListObjectChildren where
        toJSON BatchListObjectChildren'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _btchlstobjctchldrnNextToken,
                  ("MaxResults" .=) <$> _btchlstobjctchldrnMaxResults,
                  Just
                    ("ObjectReference" .=
                       _btchlstobjctchldrnObjectReference)])
