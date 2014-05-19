#!/bin/bash

rm -f /tmp/udp_openloop.*.log

n_threads=$1
client_type=$2
port=$3
server=$4
request_delay=$5
n_ops_per_client=$6
starting_id=$7

my_starting_id=$starting_id
for n in `seq $1`; do
    #echo Running $client_type $port $server $request_delay $n_ops_per_process $my_starting_id
    $client_type $port $server $request_delay $n_ops_per_client $my_starting_id > /tmp/udp_openloop.$n.log &
    let my_starting_id=$my_starting_id+$n_ops_per_client
done
wait

cat /tmp/udp_openloop.*.log
