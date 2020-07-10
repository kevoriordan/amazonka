{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchListObjectParents
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudDirectory.Types.BatchListObjectParents where

import Network.AWS.CloudDirectory.Types.ObjectReference
import Network.AWS.Lens
import Network.AWS.Prelude

-- | /See:/ 'batchListObjectParents' smart constructor.
data BatchListObjectParents = BatchListObjectParents'{_btchlstobjctprntsNextToken
                                                      :: !(Maybe Text),
                                                      _btchlstobjctprntsMaxResults
                                                      :: !(Maybe Nat),
                                                      _btchlstobjctprntsObjectReference
                                                      :: !ObjectReference}
                                deriving (Eq, Read, Show, Data, Typeable,
                                          Generic)

-- | Creates a value of 'BatchListObjectParents' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'btchlstobjctprntsNextToken' - Undocumented member.
--
-- * 'btchlstobjctprntsMaxResults' - Undocumented member.
--
-- * 'btchlstobjctprntsObjectReference' - Undocumented member.
batchListObjectParents
    :: ObjectReference -- ^ 'btchlstobjctprntsObjectReference'
    -> BatchListObjectParents
batchListObjectParents pObjectReference_
  = BatchListObjectParents'{_btchlstobjctprntsNextToken
                              = Nothing,
                            _btchlstobjctprntsMaxResults = Nothing,
                            _btchlstobjctprntsObjectReference =
                              pObjectReference_}

-- | Undocumented member.
btchlstobjctprntsNextToken :: Lens' BatchListObjectParents (Maybe Text)
btchlstobjctprntsNextToken = lens _btchlstobjctprntsNextToken (\ s a -> s{_btchlstobjctprntsNextToken = a})

-- | Undocumented member.
btchlstobjctprntsMaxResults :: Lens' BatchListObjectParents (Maybe Natural)
btchlstobjctprntsMaxResults = lens _btchlstobjctprntsMaxResults (\ s a -> s{_btchlstobjctprntsMaxResults = a}) . mapping _Nat

-- | Undocumented member.
btchlstobjctprntsObjectReference :: Lens' BatchListObjectParents ObjectReference
btchlstobjctprntsObjectReference = lens _btchlstobjctprntsObjectReference (\ s a -> s{_btchlstobjctprntsObjectReference = a})

instance Hashable BatchListObjectParents where

instance NFData BatchListObjectParents where

instance ToJSON BatchListObjectParents where
        toJSON BatchListObjectParents'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _btchlstobjctprntsNextToken,
                  ("MaxResults" .=) <$> _btchlstobjctprntsMaxResults,
                  Just
                    ("ObjectReference" .=
                       _btchlstobjctprntsObjectReference)])
