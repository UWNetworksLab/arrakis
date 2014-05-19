#!/usr/bin/env python

import sys
import os
import re

TRACE_SUBSYS_NNET = 0x9000
TRACE_SUBSYS_NET = 0x6000
TRACE_SUBSYS_LLNET = 0x6000
valid_cores = ["3 ", "2 "]
core_map = {2: "DRV", 3:"APP"}

# Following constans are used for profiling modified stack
event_map = {
        0x0001 : "Start",
        0x0002 : "Stop",
        0x0003 : "Driver saw pkg (RX)",
        0x0004 : "Ethersrv saw pkg",
        0x0005 : "Ethersrv checked frag",
        0x0006 : "Ethersrv app filtered",
        0x0007 : "Ethersrv app c2u started",
        0x0008 : "Ethersrv copied pkg",
        0x000D : "Ethersrv spp produce done",
        0x0009 : "Ethersrv app notify",
        0x000A : "LWIP handle_incoming_",
        0x000B : "LWIP call rec_handler",
        0x000C : "APP received",

        0x0020 : "APP sent",
        0x0021 : "LWIP idc_send_packet",
        0x0029 : "LWIP before mfence",
        0x002A : "LWIP after mfence",
        0x002B : "LWIP payload flushed",
        0x002C : "LWIP bufferdesc fetched",
        0x0022 : "LWIP spp produced",
        0x0023 : "LWIP update spp index",
        0x002D : "LWIP pending TX work",
        0x0024 : "LWIP notify driver",
        0x0025 : "Ethersrv notify recieved",
        0x002E : "Ethersrv send_pkt_on_w..",
        0x0026 : "Ethersrv send_sng_pkt..",
        0x0027 : "Driver add pkg (TX)",
        0x0028 : "Driver saw pkg done (TX)"
        }


event_map1 = {
0X0001: "NET_START",
0X0002: "NET_STOP",
0X0003: "New_packet_came",
0X0004: "pkt_processed",
0X0005: "pkt_uploaded",
0X0006: "pkt_entered_app",
0X0007: "pkt_processed_app",
0X0008: "reply_prepared_app",
0X0009: "reply_queued_app",
0X000A: "reply_sent_app",
0X000B: "reply_into_NIC",
0X000C: "reply_sent_by_NIC",
0x000D: "app_done_with_pbuf",
0x000E: "app_recved_TX_done",
0x0010: "interrupt_came",
0x0011: "ARP_packet_incoming",
0x0012: "physical_interrupt_came",
0x0013: "TX_Done_by_NIC",
0x0014: "register_pbuf_in_NIC",
0x0015: "filters_executed_1",
0x0016: "filters_executed_2",
0x0017: "packet_copied",
0x0018: "filter_frag",
0x0019: "packet_copy_started",
0x001A: "packet_copied_src_read",
0x001B: "packet_copied_dst_read",
0x001C: "packet_copy_copied",


}

kmap   = { 0xcccc: 'Context switch',
           0xed00: 'Sched make runnable',
           0xed01: 'Sched remove',
           0xed02: 'Sched yield',
           0xed02: 'Sched schedule',
           0xed04: 'Sched current',
         }

tmap   = { 0x0400: 'Sem wait enter',
           0x0401: 'Sem wait leave',
           0x0402: 'Sem try wait',
           0x0403: 'Sem post',
         }

netmap = { 0x0001: 'Start',
           0x0002: 'Stop',

           0x0003: 'Driver saw pkg (RX)',
           0x0004: 'Ethersrv saw pkg',
           0x0005: 'Ethersrv checked frag',
           0x0006: 'Ethersrv app filtered',
           0x0007: 'Ethersrv app c2u started',
           0x0008: 'Ethersrv copied pkg',
           0x0009: 'Ethersrv app notify',
           0x000A: 'LWIP handle_incoming_',
           0x000B: 'LWIP call rec_handler',
           0x000C: 'APP recieved',
           0x000D: 'Ethersrv spp produce done',

           0x0020: 'APP sent',
           0x0021: 'LWIP idc_send_packet',
           0x0029: 'LWIP before mfence',
           0x002A: 'LWIP after mfence',
           0x002B: 'LWIP payload flushed',
           0x002C: 'LWIP bufferdesc fetched',
           0x0022: 'LWIP spp produced',
           0x0023: 'LWIP update spp index',
           0x002D: 'LWIP pending TX work',
           0x0024: 'LWIP notify driver',
           0x0025: 'Ethersrv notify recieved',
           0x002E: 'Ethersrv send_pkt_on_w..',
           0x0026: 'Ethersrv send_sng_pkt..',
           0x0027: 'Driver add pkg (TX)',
           0x0028: 'Driver saw pkg done (TX)',

           0x0030: 'tcp_write done',
           0x0031: 'tcp_output done',
           0x0032: 'tcp_recved done',
           0x0033: 'tx pbuf freed',
           0x0034: 'tx pbuf memp start',
           0x0035: 'tx pbuf memp done',
           }



llmap = {  0x0001: 'LLNET_START',
           0x0002: 'LLNET_STOP',
           0x0003: 'LLNET_IRQ',

           0x0010: 'LLNET_DRVIRQ',
           0x0011: 'LLNET_DRVRX',
           0x0012: 'LLNET_DRVTXADD',
           0x0013: 'LLNET_DRVTXDONE',

           0x0020: 'LLNET_LWIPPBA1',
           0x0021: 'LLNET_LWIPPBA2',
           0x0022: 'LLNET_LWIPPBF1',
           0x0023: 'LLNET_LWIPPBF2',
           0x0024: 'LLNET_LWIPRX',
           0x0025: 'LLNET_LWIPTX',

           0x0030: 'LLNET_APPRX',
           0x0031: 'LLNET_APPTX',
        }


def default_info(ev):
	return ("%-15x" % ev['INFO'])

def kern_info(ev):
	if ev['EVENT'] == 0xcccc:
		return DCB[ev['CID']][ev['INFO']]
	return default_info(ev)

ssmap  = { 0xffff: ('KERN', kmap, kern_info),
           0xeeee: ('THREAD', tmap, default_info),
           0x9000: ('NET', netmap, default_info),
           0xd000: ('LLNET', llmap, default_info),
           0xea00: ('UMP TX', {}, default_info),
           0xeb00: ('UMP RX', {}, default_info),
         }


NIC_IN_EVENTS = {
0x0012: "physical interrupt came",
0x0010: "interrupt came",
0X0003: "New packet came",
0X0004: "pkt processed",
0X0005: "pkt uploaded"
}

APP_IN_EVENTS = {
0X0006: "pkt entered app",
0X0007: "pkt processed app"
}

APP_OUT_EVENTS = {
0X0008: "reply prepared app",
0X0009: "reply queued app",
0X000A: "reply sent app"
}

NIC_OUT_EVENTS = {
0X000B: "reply into NIC",
0X000C: "reply sent by NIC"
}

machine_speeds = {
        'nos6' : 2799.877,
        'sbrinz1' : 2511.0,
        'ziger1' : 2400.367,
        'gottardo' : 1866.0,
        }
#packet_boundries = [0X0012]
#LOGICAL_ORDER = [0x012, 0x0010, 0x0003, 0x0004, 0x0005, 0x0006, 0x0007,
#				0x0008, 0x0009, 0x000A, 0x000b, 0x000c]

packet_boundries = [0x0031]
LOGICAL_ORDER = [0x0010, 0x0003, 0x0015, 0x0016, # 0x0019, 0x001a, 0x001b, 0x001c,
				0x0017, 0x0004, 0x0005, 0x0006, 0x0007, 0x0008, 0x0009, 0x000A,
				0x000b, 0x000c]

#LOGICAL_ORDER = [0x0010, 0x0003, 0x0015, 0x0016, 0x0019, 0x001a, 0x001b, 0x001c,
#				0x0017, 0x0004, 0x0005, 0x0006, 0x0007, 0x0008, 0x0009, 0x000A,
#				0x000b, 0x000c]



# Maps core to (map of context id -> name)
DCB = {}


MAX_EVENT_NOREPLY = 9
MAX_EVENTS = 0x00FF

DEBUG = False
DEBUG = True

MACHINE = 'sbrinz1'

def dprint (args):
	if DEBUG:
		print args

def c2t (cycles):
#	return (cycles)  # cycles
	return ( cycles / (machine_speeds[MACHINE]))  # micro seconds

def print_event(event):
    ss = ssmap[event['SYS']]
    ev = event['EVENT']
    info = ss[2](event)
    if not ev in ss[1]:
        print "%-15f %-12f %-10s %-45s %-4s %s" % (c2t(event['TS']),
            c2t(event['DIFF']), ss[0], ev, core_map[event['CID']], info)
    else:
		print "%-15f %-12f %-10s %-45s %-4s %s" % (c2t(event['TS']),
			c2t(event['DIFF']), ss[0], ss[1][ev],
            core_map[event['CID']], info)
	#print event['DIFF_S'], event_map[event['EVENT']],\
	#core_map[event['CID']], event['INFO']

def show_event_list(event_list):
	for e in event_list:
		print_event(e)

def extract_events(in_f):
	splitter = re.compile(r'[\ ]')
	event_list = []
	line_count = 0
	for line in in_f:
		if line[0] == '#' :
			if line[1:6] == ' DCB ':
				c = int(line[6:8])
				i = int(line[8:24], 16) & 0xffffffff
				name = line[25:-1]
				if not c in DCB:
					DCB[c] = {}
				DCB[c][i] = name
			continue

		if line[0:2] not in valid_cores :
			continue

		tokens = splitter.split(line.strip())
		if len(tokens) != 3 :
			print "Error: Cant process line " + line
			continue
		c_event = {}
#		print "Processing line " + str(tokens)
		try:
			c_event['CID'] = int(tokens[0])
			c_event['TS'] = int(tokens[1])
			c_event['SYS'] = int(tokens[2][0:4],16)
			c_event['EVENT'] = int(tokens[2][4:8],16)
			c_event['INFO'] = int(tokens[2][8:],16)
		except Exception as inst:
			print type(inst) 	# the exception instance
			print inst.args 	# arguments stored in args
			print "exception for line " + line
			continue
#			raise


		# Lets ignore non-networking events
#		if  c_event['SYS'] != TRACE_SUBSYS_NNET :
#			print "Non network event, ignoring " + \
#                                str(c_event['SYS']) + " " + line
#			continue

		# Lets ignore start and stop events
		if c_event['EVENT'] in [0x1, 0x2]:
			continue

		if(len(event_list) == 0):
			c_event['DIFF_S'] = 0
		else:
			prev_c_event = event_list[-1]
			c_event['DIFF_S'] = c_event['TS'] - prev_c_event['TS']
		# For time being making DIFF = DIFF_S to show interesting prints
		c_event['DIFF'] = c_event['DIFF_S']
		c_event['DIFF'] = 0
		event_list.append(c_event)
	return event_list


def diff_events(event_list):
	# now sort the event list based on timestamp
	sorted_events = sorted(event_list, key=lambda dd: dd['TS'])

	packet_list = []
	i = 0
	for e in sorted_events:
		if i == 0:
			e['DIFF'] = 0
		else:
			e['DIFF'] = e['TS'] - sorted_events[i-1]['TS']
		i = i + 1
	return sorted_events

def print_packet(pkt):
	print ""
	print "######################"
	print "Time after last packet %f" % (
		c2t(pkt['PDIFF']))
	start = pkt['EL'][0]['TS']
	for e in pkt['EL']:
		a = e['TS']
		e['TS'] = a - start
		print_event(e)
		e['TS'] = a
	print "Packet Lifetime = %f, no. of events = %d" % (
	c2t(pkt['STOP'] - pkt['START']), len(pkt['EL']))


def show_packet_list(plist, only_answered=False):
	for pkt in plist:
		if (only_answered):
			if(len(pkt['EL'])<= MAX_EVENT_NOREPLY):
				continue
		print_packet(pkt)

def find_event_in_pkt(event_id, pkt):
	for e in pkt['EL']:
		if event_id == e['EVENT']:
			return e
	return None

def add_packet_stat(pkt, stat):
	stat['PKTS'] = stat['PKTS'] + 1

	# calculate packet lifetime
	lifetime = pkt['EL'][-1]['TS'] - pkt['EL'][0]['TS']
	if stat['PKTS'] == 1 :
		stat['PLIFE_SUM'] = lifetime
		stat['PLIFE_MAX'] = lifetime
		stat['PLIFE_MIN'] = lifetime
	else:
		stat['PLIFE_SUM'] = stat['PLIFE_SUM'] + lifetime
		if lifetime < stat['PLIFE_MIN']:
			stat['PLIFE_MIN'] = lifetime
		if lifetime > stat['PLIFE_MAX']:
			stat['PLIFE_MAX'] = lifetime

#	stat['PLIFE'] = stat['PLIFE'] + ( pkt['EL'][-1]['TS'] - pkt['EL'][0]['TS'])

	prev = None
	for i in LOGICAL_ORDER:
		e = find_event_in_pkt(i, pkt)
		if e == None:
			print "event does not exist in packet!!!!"
			print "event it %d " % (i)
			print "pkt event list is " + str(pkt['EL'])
			sys.exit(1) #continue
		stat['COUNT'][i] = stat['COUNT'][i] + 1
		if prev == None:
			# This is the first event in the packet
			diff = 0
		else:
			diff = e['TS'] - prev['TS']


		if stat['PKTS'] == 1 :
			# This is the first packet
			stat['MAX'][i] = diff
			stat['MIN'][i] = diff
			stat['SUM'][i] = diff
		else:
			stat['SUM'][i] = stat['SUM'][i] + diff
			if (diff < stat['MIN'][i]):
				stat['MIN'][i] = diff
			if (diff > stat['MAX'][i]):
				stat['MAX'][i] = diff
		prev = e


#	for e in pkt['EL']:
#		print "event is %d " %(e['EVENT'])
#		stat['COUNT'][e['EVENT']] = stat['COUNT'][e['EVENT']] + 1
#		stat['SUM'][e['EVENT']] = stat['SUM'][e['EVENT']] + e['DIFF']

def print_stats(stat):
	print "#################################"
	print "######## AVERAGE of pkts %d #####" % (stat['PKTS'])
	print "#################################"
	if stat['PKTS'] == 0 :
		print "No answered packets!!"
		return

	for i in LOGICAL_ORDER:
		if stat['COUNT'][i] == 0 :
			continue
		avg = stat['SUM'][i] / stat['COUNT'][i]

		print "e %-25s %5d %10f %10f %10f" % (event_map[i], stat['COUNT'][i],
							c2t(stat['MIN'][i]), c2t(avg), c2t(stat['MAX'][i]))

#		print "e %s %d %f %f %f" % (event_map[i], stat['COUNT'][i],
#							c2t(stat['MIN'][i]), c2t(avg), c2t(stat['MAX'][i]))

	print "Packet lifespan %f/%f/%f" % (c2t(stat['PLIFE_MIN']),
				c2t(stat['PLIFE_SUM']/stat['PKTS']), c2t(stat['PLIFE_MAX']))

	print "######################"
	print "######################"

def create_packet_list_stat():
	plist_stat = {}
	plist_stat['PKTS'] = 0
	plist_stat['PDIFF'] = 0
	plist_stat['PLIFE'] = 0
	plist_stat['COUNT'] = []
	plist_stat['SUM'] = []
	plist_stat['MAX'] = []
	plist_stat['MIN'] = []


	for i in range(0x0001,MAX_EVENTS+1):
		plist_stat['SUM'].append(0) #[i] = 0
		plist_stat['COUNT'].append(0) #[i] = 0
		plist_stat['MAX'].append(0) #[i] = 0
		plist_stat['MIN'].append(0) #[i] = 0
	return plist_stat

def is_answered_packet(pkt):
	if len(pkt['EL']) < len(LOGICAL_ORDER):
		dprint("not answered pkt as len  %d is smaller than %d" % (
							len(pkt['EL']), len(LOGICAL_ORDER)) )
		return False
	counter = 0
	for e in pkt['EL']:
		if e['EVENT'] in LOGICAL_ORDER :
			counter = counter + 1
	if counter == len(LOGICAL_ORDER):
		dprint("Answered..!!!!")
		return True

	dprint("Not answered as common events are %d instead of %d"%(counter,
						len(LOGICAL_ORDER)))
	return False

def show_answered_packets_list(plist):
	if plist == None:
		print "No packets.."
		return
	if len(plist) == 0:
		print "No packets..."
		return
	stat = create_packet_list_stat()
	for pkt in plist:
		if is_answered_packet(pkt):
			dprint("there are answered packets!!!")
			add_packet_stat(pkt, stat)
	print_stats(stat)


def create_empty_packet(starting_event, last_stop):
	starting_event['DIFF'] = 0
	pkt = {'EL':		[starting_event],
		'START':	starting_event['TS'],
		}
	if last_stop == 0:
		pkt['PDIFF'] = 0
	else:
		pkt['PDIFF'] = pkt['START'] - last_stop

	return pkt

def group_events(event_list):
	packet_list = []
	i = 0
	for e in event_list:
		is_appended = False
		if i == 0:
			packet_list.append(create_empty_packet(e, 0))
			is_appended = True
		else:
			if e['EVENT'] in packet_boundries :
				packet_list.append(create_empty_packet(e,
				packet_list[-1]['STOP']))
				is_appended = True

		packet_list[-1]['STOP'] = e['TS']
		e['PDIFF'] = e['TS'] - packet_list[-1]['EL'][-1]['TS']
		if not is_appended :
			packet_list[-1]['EL'].append(e)
		i = i + 1

	return packet_list

def process_trace(in_f):
	elist = diff_events(extract_events(in_f))
	dprint("no. of events detected is " + str(len(elist)))
#	show_event_list(elist)

	plist = group_events(elist)
#	dprint("no. of packets detected is " + str(len(plist)))

	show_packet_list(plist)
#	show_packet_list(plist, True)
#	show_answered_packets_list(plist)

def show_usage():
	print "Usage: " + sys.argv[0] + " <traceFile>"


def main():
	if len(sys.argv) != 2:
		show_usage()
		sys.exit(1)
	inputFile = open(sys.argv[1], 'r')
	#outputFile = open(sys.argv[2], 'w')
	process_trace(inputFile)

if __name__ == '__main__' :
	main()
