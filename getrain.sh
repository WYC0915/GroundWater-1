#!/bin/sh
OUT=$(tail -n 1 ./recordlist.csv)
OLD_IFS="$IFS"
IFS=","
array=($OUT)
IFS="$OLD_IFS"

#echo ${array[0]},,,${array[1]}
#declare -i year=${array[0]}
#declare -i month=${array[1]}
#if [ "$1" = "123" ]
for ((years=${array[0]};years<2022;years=years+1))
do
    for ((months=1;months<=12;months=months+1))
    do
        if [[ $years = ${array[0]} && $((months+0)) -le $((array[1]+0)) ]]
        then
            #echo "$years","$months"
            #echo "0+$months","0+${array[1]}"
            continue
        else
            echo "$years","$months"
            lines=$(curl -Is "https://gweb.wra.gov.tw/HydroInfo/RealTime/DownLoadCsvByName?fileName=RA_$years$months.csv" | sed -n 3p | tr -d '\r\n' | cut -d ' ' -f 2)
            #lines=$(curl -Is "https://gweb.wra.gov.tw/HydroInfo/RealTime/DownLoadCsvByName?fileName=RA_$years$months.csv" | grep 'content-length:' )
            #echo "checkpoint5", $lines,"test"
            if [[ $lines -eq "0" ]]
            then
                echo "no content",$lines
                
            else
                curl "https://gweb.wra.gov.tw/HydroInfo/RealTime/DownLoadCsvByName?fileName=RA_$years$months.csv" >> $years$months.csv
                python wraGwToInfluxDB.py $years$months.csv
                echo $years,$months >> ./recordlist.csv
                rm $years$months.csv
            fi
             
               
            #curl -s "https://gweb.wra.gov.tw/HydroInfo/RealTime/DownLoadCsvByName?fileName=RA_$years$months.csv" >> $years$months.csv
            #echo "https://gweb.wra.gov.tw/HydroInfo/RealTime/DownLoadCsvByName?fileName=RA_$years$months.csv"
        fi
        #echo "$years","$months"
    done
done
#    echo "https://gweb.wra.gov.tw/HydroInfo/RealTime/DownLoadCsvByName?fileName=RA_2020$month.csv"
#done  



#cat ~/recordlist.csv | while read line
#do
    #echo ${line}
    #對IFS變數 進行替換處理
#    OLD_IFS="$IFS"
#    IFS=","
#    array=($line)
#    IFS="$OLD_IFS"

#    if [$year = ]
#    echo "https://gweb.wra.gov.tw/HydroInfo/RealTime/DownLoadCsvByName?fileName=RA_${array[0]}${array[1]}.csv"
    #echo ${array[0]},,,${array[1]}
    
#done


