
# load workspace
WORKSPACE=$(az config get --query "defaults[?name=='workspace'].value" -o tsv)

# get registry name
ACR_NAME=$(az ml workspace show -n $WORKSPACE --query container_registry -o tsv | cut -d "/" -f9-)

# image tag 
IMAGE_TAG= ${ACR_NAME}.azurecr.io/bikeshare-acr

# build acr 
az acr build ./src -t $IMAGE_TAG -r $ACR_NAME 

# print the image name 
echo $IMAGE_TAG