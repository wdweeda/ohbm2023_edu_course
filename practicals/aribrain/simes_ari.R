library('ARIbrain')

#read in the files
pval <- RNifti::readNifti('p.nii.gz')
zstat <- RNifti::readNifti('z.nii.gz')
mask <- RNifti::readNifti('mask.nii.gz')

#estimate clusters
clus <- ARIbrain::cluster_threshold(zstat>3.1)

#ari output providing clusters
ari_out <- ARIbrain::ARI(Pmap = pval, 
                         clusters = clus, 
                         mask = mask, 
                         Statmap = zstat, alpha = 0.05)

#write the cluster to nifti
RNifti::writeNifti(clus, file = 'cluster31.nii.gz', template = zstat)

#get clusters with at least a certain TDP (in this case 0.7)
ari_tdp <- ARIbrain::TDPQuery(ARIbrain::ARIBrainCluster(Pmap = pval, mask = mask, alpha = 0.05), threshold = .7)

#write the clusters to a nifti file (it is a file similar to a cluster index)
ARIbrain::writeClusters(ari_tdp, file = 'tdp07clusters.nii.gz', template = zstat)

