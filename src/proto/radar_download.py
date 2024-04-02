import boto3
import botocore
import requests

working_link = "https://noaa-nexrad-level2.s3.amazonaws.com/2024/03/23/KDIX/KDIX20240323_230235_V06"

bucket_name = 'noaa-nexrad-level2'
bucket_url = f'https://{bucket_name}.s3.amazonaws.com/'

year = "2024"
month = "03"
day = "23"
filename = "KDIX20240323_230235_V06"
station = "KDIX"

# headers = {'Host': f'{bucket_name}.s3.us-east-1.amazonaws.com'}

def build_url(bucket_url, year, month, day, station, filename):
    return f"{bucket_url}{year}/{month}/{day}/{station}/{filename}"

built_url = build_url(bucket_url, year, month, day, station, filename)

r = requests.get(built_url)
print(r.status_code)
print(r.headers)

# Create an S3 client
s3 = boto3.client('s3')

# this works
r = requests.get("https://noaa-nexrad-level2.s3.amazonaws.com/2024/03/23/KFDR/KFDR20240323_055010_V06")


try:
    # Download the file using the public URL
    s3.download_file(bucket_name, file_path, 'downloaded_file.gz')
    print("File downloaded successfully.")
    
except botocore.exceptions.ClientError as e:
    if e.response['Error']['Code'] == "404":
        print(f"The file does not exist: {public_url}")
    else:
        print(f"Error downloading the file: {e}")
