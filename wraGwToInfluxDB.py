from os import times
import timeit
import requests
import influxdb_client
import os
from influxdb_client.client.write_api import SYNCHRONOUS
from influxdb_client import InfluxDBClient, Point, WriteOptions
import json
from datetime import datetime,timezone
import csv
import argparse
def main():
    from requests.models import parse_url
    import json
    import os

    from pytz import UTC
    from dateutil.parser import parse

    from datetime import datetime

    import pandas as pd
    import copy
    deltTime=60*60*8
    EPOCH = UTC.localize(datetime.utcfromtimestamp(deltTime))
    parser = argparse.ArgumentParser()
    parser.add_argument("path", help="20216.csv",type=str)
    args = parser.parse_args()
    file_path = args.path# 秀潭GNSS檔案位置,須依照實際路徑更改
    print("readfile=",file_path)
#    df_t = pd.read_csv(file_path, encoding='big5',dtype={'站名':str,})
    
#    url='192.168.0.32'
#    port=8086
#    db='water'
#    MEASUREMENT='isochrone'
#    url_string = 'http://'+str(url)+':'+str(port)+'/write?db='+str(db)

    #influx setup  start
    bucket_name = "web"
    token = "yZHMctXh5SyxfGL4QgvooxqOwfphAkQwkBwAtpT2BzzzoG5qEWIfFfB2NkfVGfOh0K_dUetKm4_KsCa1rZ2b4w=="
    org = "AITLab"
    url="http://192.168.0.32:8086"
    orgid = "b3b26dda5ba9a72d"
    client = influxdb_client.InfluxDBClient(
        url=url,
        token=token,
        org=org
    )
    if not influxdb_client.BucketsApi(client).find_bucket_by_name(bucket_name):
        influxdb_client.BucketsApi(client).create_bucket(bucket_name=bucket_name, org_id=orgid, retention_rules=None, description=None)
    write_api = client.write_api(write_options=WriteOptions(batch_size=500,
                                                      flush_interval=10_000,
                                                      jitter_interval=2_000,
                                                      retry_interval=5_000,
                                                      max_retries=5,
                                                      max_retry_delay=30_000,
                                                      exponential_base=2))
#(write_options=SYNCHRONOUS)
    #influx setup end

    with open(file_path, newline='',encoding="Big5") as csvfile:
        #i=0
        rows = csv.reader(csvfile)
        next(rows,None)
        rowdata = {}
        for row in rows:
            #print(i)
            #i=i+1
            rowdata["measurement"]="及時雨量"
            rowdata["tags"]={}
            rowdata["fields"]={}
            rowdata["tags"]["ST_NO"]=str(row[1]).replace(' ','')
            rowdata["tags"]["ST_NO1"]=str(row[2]).replace(' ','')
            rowdata["tags"]["NAME_C"]=str(row[3]).replace(' ','')
            rowdata["fields"]["ELEV_m"]=float(row[5])
            rowdata["fields"]["Rain_mm"]=float(row[6])
            #temptime=datetime.strptime(str(row[4]), "%Y-%m-%d %H:%M").replace(tzinfo=timezone.utc).timestamp()
            temptime=datetime.strptime(str(row[4]), "%Y-%m-%d %H:%M").isoformat()
            rowdata["time"]=temptime
            #print(rowdata)
            write_api.write(bucket=bucket_name, org=org, record=rowdata)

if __name__ == "__main__":
    start = timeit.default_timer()
    main()
    stop = timeit.default_timer()
    print('Time: ', stop - start)
