{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ParameterStringFilter
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.ParameterStringFilter where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | One or more filters. Use a filter to return a more specific list of results.
--
--
-- /Important:/ The @ParameterStringFilter@ object is used by the 'DescribeParameters' and 'GetParametersByPath' API actions. However, not all of the pattern values listed for @Key@ can be used with both actions.
--
-- For @DescribeActions@ , all of the listed patterns are valid, with the exception of @Label@ .
--
-- For @GetParametersByPath@ , the following patterns listed for @Key@ are not valid: @Name@ , @Path@ , and @Tier@ .
--
-- For examples of CLI commands demonstrating valid parameter filter constructions, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/parameter-search.html Searching for Systems Manager parameters> in the /AWS Systems Manager User Guide/ .
--
--
-- /See:/ 'parameterStringFilter' smart constructor.
data ParameterStringFilter = ParameterStringFilter'{_psfValues
                                                    :: !(Maybe (List1 Text)),
                                                    _psfOption :: !(Maybe Text),
                                                    _psfKey :: !Text}
                               deriving (Eq, Read, Show, Data, Typeable,
                                         Generic)

-- | Creates a value of 'ParameterStringFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psfValues' - The value you want to search for.
--
-- * 'psfOption' - For all filters used with 'DescribeParameters' , valid options include @Equals@ and @BeginsWith@ . The @Name@ filter additionally supports the @Contains@ option. (Exception: For filters using the key @Path@ , valid options include @Recursive@ and @OneLevel@ .) For filters used with 'GetParametersByPath' , valid options include @Equals@ and @BeginsWith@ . (Exception: For filters using the key @Label@ , the only valid option is @Equals@ .)
--
-- * 'psfKey' - The name of the filter.
parameterStringFilter
    :: Text -- ^ 'psfKey'
    -> ParameterStringFilter
parameterStringFilter pKey_
  = ParameterStringFilter'{_psfValues = Nothing,
                           _psfOption = Nothing, _psfKey = pKey_}

-- | The value you want to search for.
psfValues :: Lens' ParameterStringFilter (Maybe (NonEmpty Text))
psfValues = lens _psfValues (\ s a -> s{_psfValues = a}) . mapping _List1

-- | For all filters used with 'DescribeParameters' , valid options include @Equals@ and @BeginsWith@ . The @Name@ filter additionally supports the @Contains@ option. (Exception: For filters using the key @Path@ , valid options include @Recursive@ and @OneLevel@ .) For filters used with 'GetParametersByPath' , valid options include @Equals@ and @BeginsWith@ . (Exception: For filters using the key @Label@ , the only valid option is @Equals@ .)
psfOption :: Lens' ParameterStringFilter (Maybe Text)
psfOption = lens _psfOption (\ s a -> s{_psfOption = a})

-- | The name of the filter.
psfKey :: Lens' ParameterStringFilter Text
psfKey = lens _psfKey (\ s a -> s{_psfKey = a})

instance Hashable ParameterStringFilter where

instance NFData ParameterStringFilter where

instance ToJSON ParameterStringFilter where
        toJSON ParameterStringFilter'{..}
          = object
              (catMaybes
                 [("Values" .=) <$> _psfValues,
                  ("Option" .=) <$> _psfOption,
                  Just ("Key" .= _psfKey)])
