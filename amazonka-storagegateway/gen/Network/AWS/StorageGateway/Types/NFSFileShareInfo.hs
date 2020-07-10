{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.NFSFileShareInfo
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.StorageGateway.Types.NFSFileShareInfo where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.StorageGateway.Types.NFSFileShareDefaults
import Network.AWS.StorageGateway.Types.ObjectACL
import Network.AWS.StorageGateway.Types.Tag

-- | The Unix file permissions and ownership information assigned, by default, to native S3 objects when file gateway discovers them in S3 buckets. This operation is only supported in file gateways.
--
--
--
-- /See:/ 'nFSFileShareInfo' smart constructor.
data NFSFileShareInfo = NFSFileShareInfo'{_nfsfsiFileShareStatus
                                          :: !(Maybe Text),
                                          _nfsfsiKMSKey :: !(Maybe Text),
                                          _nfsfsiGatewayARN :: !(Maybe Text),
                                          _nfsfsiPath :: !(Maybe Text),
                                          _nfsfsiObjectACL ::
                                          !(Maybe ObjectACL),
                                          _nfsfsiKMSEncrypted :: !(Maybe Bool),
                                          _nfsfsiFileShareId :: !(Maybe Text),
                                          _nfsfsiFileShareARN :: !(Maybe Text),
                                          _nfsfsiDefaultStorageClass ::
                                          !(Maybe Text),
                                          _nfsfsiRole :: !(Maybe Text),
                                          _nfsfsiSquash :: !(Maybe Text),
                                          _nfsfsiRequesterPays :: !(Maybe Bool),
                                          _nfsfsiNFSFileShareDefaults ::
                                          !(Maybe NFSFileShareDefaults),
                                          _nfsfsiLocationARN :: !(Maybe Text),
                                          _nfsfsiClientList ::
                                          !(Maybe (List1 Text)),
                                          _nfsfsiGuessMIMETypeEnabled ::
                                          !(Maybe Bool),
                                          _nfsfsiReadOnly :: !(Maybe Bool),
                                          _nfsfsiTags :: !(Maybe [Tag])}
                          deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NFSFileShareInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nfsfsiFileShareStatus' - Undocumented member.
--
-- * 'nfsfsiKMSKey' - Undocumented member.
--
-- * 'nfsfsiGatewayARN' - Undocumented member.
--
-- * 'nfsfsiPath' - Undocumented member.
--
-- * 'nfsfsiObjectACL' - Undocumented member.
--
-- * 'nfsfsiKMSEncrypted' - True to use Amazon S3 server-side encryption with your own AWS KMS key, or false to use a key managed by Amazon S3. Optional. 
--
-- * 'nfsfsiFileShareId' - Undocumented member.
--
-- * 'nfsfsiFileShareARN' - Undocumented member.
--
-- * 'nfsfsiDefaultStorageClass' - The default storage class for objects put into an Amazon S3 bucket by the file gateway. Possible values are @S3_STANDARD@ , @S3_STANDARD_IA@ , or @S3_ONEZONE_IA@ . If this field is not populated, the default value @S3_STANDARD@ is used. Optional.
--
-- * 'nfsfsiRole' - Undocumented member.
--
-- * 'nfsfsiSquash' - Undocumented member.
--
-- * 'nfsfsiRequesterPays' - A value that sets who pays the cost of the request and the cost associated with data download from the S3 bucket. If this value is set to true, the requester pays the costs. Otherwise the S3 bucket owner pays. However, the S3 bucket owner always pays the cost of storing data.
--
-- * 'nfsfsiNFSFileShareDefaults' - Undocumented member.
--
-- * 'nfsfsiLocationARN' - Undocumented member.
--
-- * 'nfsfsiClientList' - Undocumented member.
--
-- * 'nfsfsiGuessMIMETypeEnabled' - A value that enables guessing of the MIME type for uploaded objects based on file extensions. Set this value to true to enable MIME type guessing, and otherwise to false. The default value is true.
--
-- * 'nfsfsiReadOnly' - A value that sets the write status of a file share. This value is true if the write status is read-only, and otherwise false.
--
-- * 'nfsfsiTags' - A list of up to 50 tags assigned to the NFS file share, sorted alphabetically by key name. Each tag is a key-value pair. For a gateway with more than 10 tags assigned, you can view all tags using the @ListTagsForResource@ API operation.
nFSFileShareInfo
    :: NFSFileShareInfo
nFSFileShareInfo
  = NFSFileShareInfo'{_nfsfsiFileShareStatus = Nothing,
                      _nfsfsiKMSKey = Nothing, _nfsfsiGatewayARN = Nothing,
                      _nfsfsiPath = Nothing, _nfsfsiObjectACL = Nothing,
                      _nfsfsiKMSEncrypted = Nothing,
                      _nfsfsiFileShareId = Nothing,
                      _nfsfsiFileShareARN = Nothing,
                      _nfsfsiDefaultStorageClass = Nothing,
                      _nfsfsiRole = Nothing, _nfsfsiSquash = Nothing,
                      _nfsfsiRequesterPays = Nothing,
                      _nfsfsiNFSFileShareDefaults = Nothing,
                      _nfsfsiLocationARN = Nothing,
                      _nfsfsiClientList = Nothing,
                      _nfsfsiGuessMIMETypeEnabled = Nothing,
                      _nfsfsiReadOnly = Nothing, _nfsfsiTags = Nothing}

-- | Undocumented member.
nfsfsiFileShareStatus :: Lens' NFSFileShareInfo (Maybe Text)
nfsfsiFileShareStatus = lens _nfsfsiFileShareStatus (\ s a -> s{_nfsfsiFileShareStatus = a})

-- | Undocumented member.
nfsfsiKMSKey :: Lens' NFSFileShareInfo (Maybe Text)
nfsfsiKMSKey = lens _nfsfsiKMSKey (\ s a -> s{_nfsfsiKMSKey = a})

-- | Undocumented member.
nfsfsiGatewayARN :: Lens' NFSFileShareInfo (Maybe Text)
nfsfsiGatewayARN = lens _nfsfsiGatewayARN (\ s a -> s{_nfsfsiGatewayARN = a})

-- | Undocumented member.
nfsfsiPath :: Lens' NFSFileShareInfo (Maybe Text)
nfsfsiPath = lens _nfsfsiPath (\ s a -> s{_nfsfsiPath = a})

-- | Undocumented member.
nfsfsiObjectACL :: Lens' NFSFileShareInfo (Maybe ObjectACL)
nfsfsiObjectACL = lens _nfsfsiObjectACL (\ s a -> s{_nfsfsiObjectACL = a})

-- | True to use Amazon S3 server-side encryption with your own AWS KMS key, or false to use a key managed by Amazon S3. Optional. 
nfsfsiKMSEncrypted :: Lens' NFSFileShareInfo (Maybe Bool)
nfsfsiKMSEncrypted = lens _nfsfsiKMSEncrypted (\ s a -> s{_nfsfsiKMSEncrypted = a})

-- | Undocumented member.
nfsfsiFileShareId :: Lens' NFSFileShareInfo (Maybe Text)
nfsfsiFileShareId = lens _nfsfsiFileShareId (\ s a -> s{_nfsfsiFileShareId = a})

-- | Undocumented member.
nfsfsiFileShareARN :: Lens' NFSFileShareInfo (Maybe Text)
nfsfsiFileShareARN = lens _nfsfsiFileShareARN (\ s a -> s{_nfsfsiFileShareARN = a})

-- | The default storage class for objects put into an Amazon S3 bucket by the file gateway. Possible values are @S3_STANDARD@ , @S3_STANDARD_IA@ , or @S3_ONEZONE_IA@ . If this field is not populated, the default value @S3_STANDARD@ is used. Optional.
nfsfsiDefaultStorageClass :: Lens' NFSFileShareInfo (Maybe Text)
nfsfsiDefaultStorageClass = lens _nfsfsiDefaultStorageClass (\ s a -> s{_nfsfsiDefaultStorageClass = a})

-- | Undocumented member.
nfsfsiRole :: Lens' NFSFileShareInfo (Maybe Text)
nfsfsiRole = lens _nfsfsiRole (\ s a -> s{_nfsfsiRole = a})

-- | Undocumented member.
nfsfsiSquash :: Lens' NFSFileShareInfo (Maybe Text)
nfsfsiSquash = lens _nfsfsiSquash (\ s a -> s{_nfsfsiSquash = a})

-- | A value that sets who pays the cost of the request and the cost associated with data download from the S3 bucket. If this value is set to true, the requester pays the costs. Otherwise the S3 bucket owner pays. However, the S3 bucket owner always pays the cost of storing data.
nfsfsiRequesterPays :: Lens' NFSFileShareInfo (Maybe Bool)
nfsfsiRequesterPays = lens _nfsfsiRequesterPays (\ s a -> s{_nfsfsiRequesterPays = a})

-- | Undocumented member.
nfsfsiNFSFileShareDefaults :: Lens' NFSFileShareInfo (Maybe NFSFileShareDefaults)
nfsfsiNFSFileShareDefaults = lens _nfsfsiNFSFileShareDefaults (\ s a -> s{_nfsfsiNFSFileShareDefaults = a})

-- | Undocumented member.
nfsfsiLocationARN :: Lens' NFSFileShareInfo (Maybe Text)
nfsfsiLocationARN = lens _nfsfsiLocationARN (\ s a -> s{_nfsfsiLocationARN = a})

-- | Undocumented member.
nfsfsiClientList :: Lens' NFSFileShareInfo (Maybe (NonEmpty Text))
nfsfsiClientList = lens _nfsfsiClientList (\ s a -> s{_nfsfsiClientList = a}) . mapping _List1

-- | A value that enables guessing of the MIME type for uploaded objects based on file extensions. Set this value to true to enable MIME type guessing, and otherwise to false. The default value is true.
nfsfsiGuessMIMETypeEnabled :: Lens' NFSFileShareInfo (Maybe Bool)
nfsfsiGuessMIMETypeEnabled = lens _nfsfsiGuessMIMETypeEnabled (\ s a -> s{_nfsfsiGuessMIMETypeEnabled = a})

-- | A value that sets the write status of a file share. This value is true if the write status is read-only, and otherwise false.
nfsfsiReadOnly :: Lens' NFSFileShareInfo (Maybe Bool)
nfsfsiReadOnly = lens _nfsfsiReadOnly (\ s a -> s{_nfsfsiReadOnly = a})

-- | A list of up to 50 tags assigned to the NFS file share, sorted alphabetically by key name. Each tag is a key-value pair. For a gateway with more than 10 tags assigned, you can view all tags using the @ListTagsForResource@ API operation.
nfsfsiTags :: Lens' NFSFileShareInfo [Tag]
nfsfsiTags = lens _nfsfsiTags (\ s a -> s{_nfsfsiTags = a}) . _Default . _Coerce

instance FromJSON NFSFileShareInfo where
        parseJSON
          = withObject "NFSFileShareInfo"
              (\ x ->
                 NFSFileShareInfo' <$>
                   (x .:? "FileShareStatus") <*> (x .:? "KMSKey") <*>
                     (x .:? "GatewayARN")
                     <*> (x .:? "Path")
                     <*> (x .:? "ObjectACL")
                     <*> (x .:? "KMSEncrypted")
                     <*> (x .:? "FileShareId")
                     <*> (x .:? "FileShareARN")
                     <*> (x .:? "DefaultStorageClass")
                     <*> (x .:? "Role")
                     <*> (x .:? "Squash")
                     <*> (x .:? "RequesterPays")
                     <*> (x .:? "NFSFileShareDefaults")
                     <*> (x .:? "LocationARN")
                     <*> (x .:? "ClientList")
                     <*> (x .:? "GuessMIMETypeEnabled")
                     <*> (x .:? "ReadOnly")
                     <*> (x .:? "Tags" .!= mempty))

instance Hashable NFSFileShareInfo where

instance NFData NFSFileShareInfo where
