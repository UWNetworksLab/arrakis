#ifndef BENCHMARK_H
#define BENCHMARK_H

#include <bench/bench.h>

#define NUM_OF_RECORDS 64

struct stats {
	// Barrelfish -> Net
	cycles_t last_packet_receive_from_bf_ts;
	cycles_t last_packet_transmit_to_net_ts;
	cycles_t from_bf_to_net_diff[NUM_OF_RECORDS];
	int bf_to_net_i;

	//Net -> Barrelfish
	cycles_t last_packet_receive_net_ts;
	cycles_t last_packet_transmit_to_bf_ts;
	cycles_t from_net_to_bf_diff[NUM_OF_RECORDS];
	int net_to_bf_i;
};

void record_packet_receive_from_bf(void);
void record_packet_transmit_to_net(void);

void record_packet_receive_from_net(void);
void record_packet_transmit_to_bf(void);

void print_bench_stats(void);

#endif
