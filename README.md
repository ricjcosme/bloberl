bloberl: a simple man's approach to log shipping and (multi) cloud blob based storage
=====

Bloberl is an erlang app designed for log shipping and storage, based on simplicity and low resource usage.  
Although running bloberl directly in any OS that has erlang (v21.3) present is obviously possible, the goal here has been to dockerize it and deploy it via containers with the sidecar pattern. This scenario spans across a mulitude of platforms, from cloud based VMs, orchestration frameworks like Kubernetes to edge or low cost machines (yes, this thing runs both natively or in a docker container on a Raspberry Pi!).


Requirements
-----
Bloberl requires OTP version 21.3+  
Docker for the containerized scenario


Getting started
-----
I will be addressing the containerized scenario only - you can surely extrapolate the native OS deployment from there.  
Let's create the docker image
```
git clone https://github.com/ricjcosme/bloberl.git
cd bloberl
docker build . -t bloberl:latest
```
Now let's start a container from that image that ships logs to Azure Blob Storage
```
docker run --init --rm \
	-e "AZURE_STORAGE_ACCOUNT=my-bloberl-account" \
	-e "AZURE_BLOB_CONTAINER=my-bloberl-container" \
	-e 'AZURE_BLOB_STORAGE_KEY=ABCDEFGHabcdefghABCDEFGHabcdefghABCDEFGHabcdefghABCDEFGHabcdefghABCDEFGHabcdefghABCDEF==' \
	-d --name bloberl -p 31090:31090 bloberl:latest
```
As you can see we're using env variables to define Azure's account and container (AZURE_STORAGE_ACCOUNT and AZURE_BLOB_CONTAINER) and also Azure's secret key (AZURE_BLOB_STORAGE_KEY). We're also exposing bloberl tcp listener on 0.0.0.0:31090.  
Let's see a S3 example
```
docker run --init --rm \
	-e "AWS_ACCESS_KEY_ID=ABCDEFGHABCDEFGHABCD" \
	-e "AWS_SECRET_ACCESS_KEY=ABCDEFGHabcdefghABCDEFGHabcdefghABCDEFGH" \
	-e "AWS_DEFAULT_REGION=eu-west-1" \
	-e "AWS_S3_BUCKET=my-blob-logs" \
	-d --name bloberl -p 31090:31090 bloberl:latest
```
In this case we're using AWS_ACCESS_KEY_ID and AWS_SECRET_ACCESS_KEY as the AWS S3 credentials and AWS_DEFAULT_REGION as the region where our bucket is and AWS_S3_BUCKET to define the bucket name
For both AWS S3 and Azure Blob, as you can imagine, simply add both services correspondent env vars as
```
docker run --init --rm \
	-e "AZURE_STORAGE_ACCOUNT=my-bloberl-account" \
	-e "AZURE_BLOB_CONTAINER=my-bloberl-container" \
	-e 'AZURE_BLOB_STORAGE_KEY=ABCDEFGHabcdefghABCDEFGHabcdefghABCDEFGHabcdefghABCDEFGHabcdefghABCDEFGHabcdefghABCDEF==' \
	-e "AWS_ACCESS_KEY_ID=ABCDEFGHABCDEFGHABCD" \
	-e "AWS_SECRET_ACCESS_KEY=ABCDEFGHabcdefghABCDEFGHabcdefghABCDEFGH" \
	-e "AWS_DEFAULT_REGION=eu-west-1" \
	-e "AWS_S3_BUCKET=my-blob-logs" \
	-d --name bloberl -p 31090:31090 bloberl:latest
```
Here's an example for AWS S3 and Google Cloud Storage
```
docker run --init --rm \
	-e "AWS_ACCESS_KEY_ID=ABCDEFGHABCDEFGHABCD" \
	-e "AWS_SECRET_ACCESS_KEY=ABCDEFGHabcdefghABCDEFGHabcdefghABCDEFGH" \
	-e "AWS_DEFAULT_REGION=eu-west-1" \
	-e "AWS_S3_BUCKET=my-blob-logs" \
	-e "GOOGLE_CLOUD_CREDENTIALS=/bloberl/google_key.json" \
	-e "GOOGLE_CLOUD_BUCKET=bloberl-bucket" \
	-v /path/to/your/google/cloud/service/account/credentials.json:/bloberl/google_key.json \
	-d --name bloberl -p 31090:31090 bloberl:latest
```
Note that in this case - and specifically for Google Cloud - we're mapping a Google Cloud service account keys' json file ([more info here on creating and managing Google Cloud's service account keys](https://cloud.google.com/iam/docs/creating-managing-service-account-keys)) to /bloberl/google_key.json

If you do not provide one of either AWS_ACCESS_KEY_ID for S3, AZURE_STORAGE_ACCOUNT for Azure Blob or GOOGLE_CLOUD_CREDENTIALS for Google Cloud Storage, bloberl will simply exit on start. To run bloberl natively the same requirements apply regarding environment variables existance.


Environment variables
-----
| Environment variable   | Description                                                                                                              |
| ---------------------- |:-------------------------------------------------------------------------------------------------------------------------|
| AWS_ACCESS_KEY_ID      | Amazon AWS access key id credential                                                                                      |
| AWS_SECRET_ACCESS_KEY  | Amazon AWS secret key credential                                                                                         |
| AWS_DEFAULT_REGION     | Amazon AWS S3 region                                                                                                     |
| AWS_S3_BUCKET          | Amazon AWS S3 bucket name                                                                                                |
| AZURE_STORAGE_ACCOUNT  | Microsoft Azure storage account name                                                                                     |
| AZURE_BLOB_CONTAINER   | Microsoft Azure blob storage container name                                                                              |
| AZURE_BLOB_STORAGE_KEY | Microsoft Azure blob storage key credential                                                                              |
| GOOGLE_CLOUD_CREDENTIALS | Path to Google Cloud's service account keys json file                                                                  |
| GOOGLE_CLOUD_BUCKET    | Google Cloud Storage bucket name                                                                                         |
| PORT                   | bloberl internal tcp listening port (default is 31090)                                                                   |
| TCP_CLIENT_TIMEOUT     | the amount of time bloberl waits for additional data before disconnecting a client (default is 5000 - in milliseconds)   |
| MAX_RECORDS_PER_TABLE  | bloberl stores data in erlang ets tables - this establishes a max number before closing and shipping one of those tables |
| MAX_MEMORY_PER_TABLE   | just like MAX_RECORDS_PER_TABLE but for table size in memory (default is 10000000 - in bytes)                            |
| SHIPPING_INTERVAL      | the amount of time between each log data table is shipped to the cloud (default is 60000 - in milliseconds)              |


Notes
-----
- MAX_RECORDS_PER_TABLE and MAX_MEMORY_PER_TABLE are mutually exclusive - whatever comes first will cause the log data table to be closed and shipped
- Every time an ets log data table is closed a gziped file with the format 1556388465000839983.gz is created and shipped to the cloud
- This format is the UNIX Epoch time in nanoseconds: 1556388465 seconds + 000 milliseconds + 839983 nanoseconds


Goodies
-----
Assuming bloberl is running and listening on 0.0.0.0:31090 (irrespective of it being native or containerized)
- *nix is awesome, cat, netcat and logrotate are cool great tools - you can always configure a specific log file to be dumped to bloberl on pre-rotation:
```
prerotate
    cat $1 | nc -CN localhost 31090
```
- docker logging driver `syslog` works with bloberl out of the box so you can have any container logging to bloberl like this:
```
docker run -ti --rm --log-driver=syslog --log-opt tag=docker.{{.ID}} --log-opt syslog-address=tcp://0.0.0.0:31090 --name some-container some-image:latest
```
- tail -F and necat can also do the job, but I wouldn't recommend it:
```
- tail -F my-logfile.log | nc -C localhost 31090
```


Roadmap
-----
- [ ] better / full comments on the code
- [ ] better exception / error handling 
- [ ] better / more complete internal application logging
- [ ] add more storage providers (Minio, others?)


Out of scope but very interesting nonetheless when it comes to cost savvy tech
-----
[aws-cli](https://aws.amazon.com/cli/) is a tool I personally don't use much but that comes in handy every now and then.  
Now, let's imagine that we need to search our bloberl produced log files - already sitting in the cloud (S3 in this case) - for something that happened between a time range - let's imagine that that time range is from 2019-04-27 12:00:00 UTC (1556362800 unix time) to 2019-04-27 12:30:00 UTC (1556366400 unix time).  
Let's create a very handy bash script that goes something like this
```
#!/bin/bash

BUCKET=$1
for key in `aws s3api list-objects --bucket $BUCKET --prefix $2 | jq -r '.Contents[].Key'`
do
  # echo $key
  aws s3 cp s3://$BUCKET/$key - | zcat
done
```
and save it as s3zcat (don't forget to chmod +x s3zcat).  
Let's search the logs then and, as an example, let's find occurences of `PyMongo` on them - let's also assume our S3 bucket name is `my-bloberl-bucket`:
```
./s3zcat my-bloberl-bucket 155636 | grep -i PyMongo
```
In a nutshell, I'm using the prefix `155636` to select all files `--prefix $2` (this will cover the time range between 2019-04-27 11:13:20 UTC and 2019-04-27 13:43:20 UTC), cycle through each and apply zcat on it to stdout. Since I'm adding `| grep -i PyMongo` I'll get the lines where PyMongo is found in those logfiles.  
More: append `| wc -l` to the command as in `./s3zcat my-bloberl-bucket 155636 | grep -i PyMongo | wc -l` and we'll get the number of times that the text `PyMongo` showed up in our logs - gotta love *nix!
