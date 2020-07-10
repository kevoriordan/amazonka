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
-- Module      : Network.AWS.Glue.SearchTables
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Searches a set of tables based on properties in the table metadata as well as on the parent database. You can search against text or filter conditions. 
--
--
-- You can only get tables that you have access to based on the security policies defined in Lake Formation. You need at least a read-only access to the table for it to be returned. If you do not have access to all the columns in the table, these columns will not be searched against when returning the list of tables back to you. If you have access to the columns but not the data in the columns, those columns and the associated metadata for those columns will be included in the search. 
--
module Network.AWS.Glue.SearchTables
    (
    -- * Creating a Request
      searchTables
    , SearchTables
    -- * Request Lenses
    , stSearchText
    , stFilters
    , stCatalogId
    , stSortCriteria
    , stNextToken
    , stMaxResults

    -- * Destructuring the Response
    , searchTablesResponse
    , SearchTablesResponse
    -- * Response Lenses
    , stsrsTableList
    , stsrsNextToken
    , stsrsResponseStatus
    ) where

import Network.AWS.Glue.Types
import Network.AWS.Glue.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'searchTables' smart constructor.
data SearchTables = SearchTables'{_stSearchText ::
                                  !(Maybe Text),
                                  _stFilters :: !(Maybe [PropertyPredicate]),
                                  _stCatalogId :: !(Maybe Text),
                                  _stSortCriteria :: !(Maybe [SortCriterion]),
                                  _stNextToken :: !(Maybe Text),
                                  _stMaxResults :: !(Maybe Nat)}
                      deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SearchTables' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stSearchText' - A string used for a text search. Specifying a value in quotes filters based on an exact match to the value.
--
-- * 'stFilters' - A list of key-value pairs, and a comparator used to filter the search results. Returns all entities matching the predicate.
--
-- * 'stCatalogId' - A unique identifier, consisting of @/account_id/ /datalake@ .
--
-- * 'stSortCriteria' - A list of criteria for sorting the results by a field name, in an ascending or descending order.
--
-- * 'stNextToken' - A continuation token, included if this is a continuation call.
--
-- * 'stMaxResults' - The maximum number of tables to return in a single response.
searchTables
    :: SearchTables
searchTables
  = SearchTables'{_stSearchText = Nothing,
                  _stFilters = Nothing, _stCatalogId = Nothing,
                  _stSortCriteria = Nothing, _stNextToken = Nothing,
                  _stMaxResults = Nothing}

-- | A string used for a text search. Specifying a value in quotes filters based on an exact match to the value.
stSearchText :: Lens' SearchTables (Maybe Text)
stSearchText = lens _stSearchText (\ s a -> s{_stSearchText = a})

-- | A list of key-value pairs, and a comparator used to filter the search results. Returns all entities matching the predicate.
stFilters :: Lens' SearchTables [PropertyPredicate]
stFilters = lens _stFilters (\ s a -> s{_stFilters = a}) . _Default . _Coerce

-- | A unique identifier, consisting of @/account_id/ /datalake@ .
stCatalogId :: Lens' SearchTables (Maybe Text)
stCatalogId = lens _stCatalogId (\ s a -> s{_stCatalogId = a})

-- | A list of criteria for sorting the results by a field name, in an ascending or descending order.
stSortCriteria :: Lens' SearchTables [SortCriterion]
stSortCriteria = lens _stSortCriteria (\ s a -> s{_stSortCriteria = a}) . _Default . _Coerce

-- | A continuation token, included if this is a continuation call.
stNextToken :: Lens' SearchTables (Maybe Text)
stNextToken = lens _stNextToken (\ s a -> s{_stNextToken = a})

-- | The maximum number of tables to return in a single response.
stMaxResults :: Lens' SearchTables (Maybe Natural)
stMaxResults = lens _stMaxResults (\ s a -> s{_stMaxResults = a}) . mapping _Nat

instance AWSRequest SearchTables where
        type Rs SearchTables = SearchTablesResponse
        request = postJSON glue
        response
          = receiveJSON
              (\ s h x ->
                 SearchTablesResponse' <$>
                   (x .?> "TableList" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable SearchTables where

instance NFData SearchTables where

instance ToHeaders SearchTables where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSGlue.SearchTables" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON SearchTables where
        toJSON SearchTables'{..}
          = object
              (catMaybes
                 [("SearchText" .=) <$> _stSearchText,
                  ("Filters" .=) <$> _stFilters,
                  ("CatalogId" .=) <$> _stCatalogId,
                  ("SortCriteria" .=) <$> _stSortCriteria,
                  ("NextToken" .=) <$> _stNextToken,
                  ("MaxResults" .=) <$> _stMaxResults])

instance ToPath SearchTables where
        toPath = const "/"

instance ToQuery SearchTables where
        toQuery = const mempty

-- | /See:/ 'searchTablesResponse' smart constructor.
data SearchTablesResponse = SearchTablesResponse'{_stsrsTableList
                                                  :: !(Maybe [Table]),
                                                  _stsrsNextToken ::
                                                  !(Maybe Text),
                                                  _stsrsResponseStatus :: !Int}
                              deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SearchTablesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stsrsTableList' - A list of the requested @Table@ objects. The @SearchTables@ response returns only the tables that you have access to.
--
-- * 'stsrsNextToken' - A continuation token, present if the current list segment is not the last.
--
-- * 'stsrsResponseStatus' - -- | The response status code.
searchTablesResponse
    :: Int -- ^ 'stsrsResponseStatus'
    -> SearchTablesResponse
searchTablesResponse pResponseStatus_
  = SearchTablesResponse'{_stsrsTableList = Nothing,
                          _stsrsNextToken = Nothing,
                          _stsrsResponseStatus = pResponseStatus_}

-- | A list of the requested @Table@ objects. The @SearchTables@ response returns only the tables that you have access to.
stsrsTableList :: Lens' SearchTablesResponse [Table]
stsrsTableList = lens _stsrsTableList (\ s a -> s{_stsrsTableList = a}) . _Default . _Coerce

-- | A continuation token, present if the current list segment is not the last.
stsrsNextToken :: Lens' SearchTablesResponse (Maybe Text)
stsrsNextToken = lens _stsrsNextToken (\ s a -> s{_stsrsNextToken = a})

-- | -- | The response status code.
stsrsResponseStatus :: Lens' SearchTablesResponse Int
stsrsResponseStatus = lens _stsrsResponseStatus (\ s a -> s{_stsrsResponseStatus = a})

instance NFData SearchTablesResponse where
