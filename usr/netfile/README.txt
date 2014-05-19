netfile: a simple program for sending or receiving files over the network.
It's most useful to run from fish.

Usage:
netfile [-sr] [-a address] [-p port] [-f file]
-s: send file
-r: receive file
-a: IP address to send to
-p: port to send to or receive on
-f: path name of the file to send or receive to

example of sending a file: 

netfile -s -a 129.132.102.50 -p 31337 -f bootmodules

on 129.132.102.50 you can receive the file using netcat by running:

nc -l 31337

example of receiving a file:

netfile -r -p 31337 -f my_file

on another machine you can send the file using netcat by running:

nc <ip_address> 31337 < afile.txt


Note that due to the way the e1000 driver is written, netfile can 
only be run once.  Running it a second time crashes the e1000 driver.
