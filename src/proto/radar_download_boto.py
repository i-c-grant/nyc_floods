import boto3
import botocore
import wradlib as wrl
import tempfile 
import xradar as xd
import xarray as xr
bucket_name = 'noaa-nexrad-level2'

year = "2024"
month = "03"
day = "23"
station = "KDIX"

# Create an S3 client
s3 = boto3.client('s3', config=boto3.session.Config(signature_version=botocore.UNSIGNED))

# List objects in the specified prefix
prefix = f"{year}/{month}/{day}/{station}/"
response = s3.list_objects_v2(Bucket=bucket_name, Prefix=prefix)

# Download the second to last file in the list as a tempfile
file_path = response['Contents'][-3]['Key']

# Create a temporary file
tempfile = tempfile.NamedTemporaryFile(delete=False)

# Download the file using the public URL
s3.download_file(bucket_name, file_path, tempfile.name)

# Open tempfile using xradar

xr.open_dataset(tempfile.name, engine='nexradlevel2', group = 'sweep_0')
