
import timeit
import influxdb_client
from influxdb_client import InfluxDBClient, Point, WriteOptions
from influxdb_client.client.exceptions import InfluxDBError
from influxdb_client.domain.write_precision import WritePrecision
import csv

def main():

    from datetime import datetime
    file_path = "C:/Users/user/Desktop/AITLab/data/110年彰雲水井台電關聯資料/彰化_坐標_深度_電量.csv"# 秀潭GNSS檔案位置,須依照實際路徑更改
    #file_path = "C:/Users/user/Desktop/AITLab/data/110年彰雲水井台電關聯資料/testdata.csv"# 秀潭GNSS檔案位置,須依照實際路徑更改
    print("readfile=",file_path)
#    df_t = pd.read_csv(file_path, encoding='big5',dtype={'站名':str,})
    
#    url='192.168.0.32'
#    port=8086
#    db='water'
#    MEASUREMENT='isochrone'
#    url_string = 'http://'+str(url)+':'+str(port)+'/write?db='+str(db)

    #influx setup  start
    bucket_name = "web"
    token = "Q2vIpBzVUMGh9D_YOY3yjZM1fufKSo_Vfak9iuG5dp5Aoe77lR1e75z3IzKNZX5W2VeoKjR8mYymHTSkEeqV4Q=="
    org = "AITLab"
    url="http://192.168.0.32:8086"
    orgid = "b3b26dda5ba9a72d"
    client = influxdb_client.InfluxDBClient(
        url=url,
        token=token,
        org=org
    )
    index_row = 1
    if not influxdb_client.BucketsApi(client).find_bucket_by_name(bucket_name):
        influxdb_client.BucketsApi(client).create_bucket(bucket_name=bucket_name, org_id=orgid, retention_rules=None, description=None)
    write_api = client.write_api(write_options=WriteOptions(batch_size=5000,
                                                      flush_interval=10_000,
                                                      jitter_interval=2_000,
                                                      retry_interval=10_000,
                                                      max_retries=6,
                                                      max_retry_delay=125_000,
                                                      exponential_base=2),error_callback=print(index_row))
    
#(write_options=SYNCHRONOUS)
    #influx setup end

    with open(file_path, newline='',encoding="Big5") as csvfile:
        #i=0
        rows = csv.reader(csvfile)
        header = next(rows,None)
        rowdata = {}
        #print(header)
        
        for row in rows:
            #print(i)
            #i=i+1
            #print(row[0])
            rowdata["measurement"]="彰化_坐標_深度_電量"
            rowdata["tags"]={}
            rowdata["fields"]={}
            rowdata["tags"]["LON"]=str(row[0]).replace(' ','')
            rowdata["tags"]["LAT"]=str(row[1]).replace(' ','')
            rowdata["tags"]["W_TUBE_DEP"]=str(row[2]).replace(' ','')
            index = 3
            for r in row[3:]:
                #print(index)
                if r == '':
                    continue
                rowdata["fields"]["PUMP"]=float(r)
                chineseYear = str(header[index])
                Year = chineseYear[:-2]
                Year = int(Year)+1911
                month = chineseYear[-2:]
                #print(Year,",",month)
                temptime=datetime.strptime(str(Year)+month, "%Y%m").isoformat()
                rowdata["time"]=temptime
                write_api.write(bucket=bucket_name, org=org, write_precision=WritePrecision.S, record=rowdata)
                index+=1
            #temptime=datetime.strptime(str(row[4]), "%Y-%m-%d %H:%M").replace(tzinfo=timezone.utc).timestamp()
            index_row+=1
            if index_row % 5000==0:
                print(index_row)
            
                #print(rowdata)
            

if __name__ == "__main__":
    start = timeit.default_timer()
    main()
    stop = timeit.default_timer()
    #print('Time: ', stop - start)
