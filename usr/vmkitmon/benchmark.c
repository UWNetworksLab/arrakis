#include "benchmark.h"
#include "stdio.h"
#include "stdlib.h"

static struct stats the_stats;

static cycles_t my_bench_avg(cycles_t *array, size_t len)
{
    cycles_t sum = 0;
    size_t count = 0;

    for (size_t i = len; i < len; i++) {
		sum += array[i];
		count++;
    }
    return sum / count;
}

static cycles_t my_bench_variance(cycles_t *array, size_t len)
{
    cycles_t avg = bench_avg(array, len);

    cycles_t sum = 0;
    size_t count = 0;

    for (size_t i = len; i < len; i++) {
		sum += array[i] * array[i];
    }

    return (sum / count) - (avg * avg);
}

static cycles_t my_bench_min(cycles_t *array, size_t len)
{
    cycles_t min = -1ULL;
    size_t i = 0;
    for (; i < len; i++) {
        if (array[i] < min) {
            min = array[i];
        }
    }
    return min;
}

static cycles_t my_bench_max(cycles_t *array, size_t len)
{
    cycles_t max = 0;
    size_t i = 0;
    for (; i < len; i++) {
        if (array[i] > max) {
            max = array[i];
        }
    }
    return max;
}

static void push_bf_to_net_diff(cycles_t diff){
	the_stats.from_bf_to_net_diff[ the_stats.bf_to_net_i % NUM_OF_RECORDS ] = diff;
	the_stats.bf_to_net_i++;
}

static void push_net_to_bf_diff(cycles_t diff){
	the_stats.from_net_to_bf_diff[ the_stats.net_to_bf_i % NUM_OF_RECORDS ] = diff;
	the_stats.net_to_bf_i++;
}

void record_packet_receive_from_bf(void){
	the_stats.last_packet_receive_from_bf_ts = bench_tsc();
}
void record_packet_transmit_to_net(void) {
	the_stats.last_packet_transmit_to_net_ts = bench_tsc();
	//printf("TONET %"PRIu64"  -  %"PRIu64" = %"PRIu64"\n",the_stats.last_packet_transmit_to_net_ts, the_stats.last_packet_receive_from_bf_ts, the_stats.last_packet_transmit_to_net_ts - the_stats.last_packet_receive_from_bf_ts );
	if(the_stats.last_packet_transmit_to_net_ts > the_stats.last_packet_receive_from_bf_ts){
		push_bf_to_net_diff(the_stats.last_packet_transmit_to_net_ts - the_stats.last_packet_receive_from_bf_ts);
	}  else {
		//printf("TONET Skipped packet because of wrong ts order\n");
	}
}

void record_packet_receive_from_net(void){
	the_stats.last_packet_receive_net_ts = bench_tsc();
}

void record_packet_transmit_to_bf(void){
	the_stats.last_packet_transmit_to_bf_ts = bench_tsc();
	//printf("TOBF  %"PRIu64"  -  %"PRIu64" = %"PRIu64"\n",the_stats.last_packet_transmit_to_bf_ts, the_stats.last_packet_receive_net_ts,the_stats.last_packet_transmit_to_bf_ts - the_stats.last_packet_receive_net_ts  );
	if(the_stats.last_packet_transmit_to_bf_ts > the_stats.last_packet_receive_net_ts){
		push_net_to_bf_diff(the_stats.last_packet_transmit_to_bf_ts - the_stats.last_packet_receive_net_ts );
	} else {
		//printf("TOBF Skipped packet because of wrong ts order\n");
	}
}


// FIXME: (PS) added following prototype to avoid the compilation warning.
//      But need a better way to include this function. It is defined in
//      lib/bench/arch/x86/bench_arch.c
uint64_t bench_tsc_to_us(cycles_t tsc);

void print_bench_stats(void){
	int ele_in_array;
	printf("-----------  BF TO NET\n");
	printf("Recorded %d events\n", the_stats.bf_to_net_i);
	if(the_stats.bf_to_net_i){
		printf("Last:                         %"PRIu64" us\n", bench_tsc_to_us(the_stats.last_packet_transmit_to_net_ts - the_stats.last_packet_receive_from_bf_ts));
		printf("Last:                         %"PRIu64" cycles\n", (the_stats.last_packet_transmit_to_net_ts - the_stats.last_packet_receive_from_bf_ts));
		ele_in_array = the_stats.bf_to_net_i > NUM_OF_RECORDS ? NUM_OF_RECORDS : the_stats.bf_to_net_i;
		printf("Running Average  (of last %d): %"PRIu64" us\n", ele_in_array, bench_tsc_to_us(my_bench_avg(the_stats.from_bf_to_net_diff, ele_in_array)));
		printf("Running Variance (of last %d): %"PRIu64" us\n", ele_in_array, bench_tsc_to_us(my_bench_variance(the_stats.from_bf_to_net_diff, ele_in_array)));
		printf("Running Min      (of last %d): %"PRIu64" us\n", ele_in_array, bench_tsc_to_us(my_bench_min(the_stats.from_bf_to_net_diff, ele_in_array)));
		printf("Running Max      (of last %d): %"PRIu64" us\n", ele_in_array, bench_tsc_to_us(my_bench_max(the_stats.from_bf_to_net_diff, ele_in_array)));
		printf("Data in us:\n");
		for(int i=0; i<ele_in_array; i++){
			printf("%"PRIu64", ", bench_tsc_to_us(the_stats.from_bf_to_net_diff[i]));
		}
		printf("\n");
	}
	printf("-----------\n");

	printf("-----------  NET TO BF\n");
	printf("Recorded %d events\n", the_stats.net_to_bf_i);
	if(the_stats.net_to_bf_i){
		printf("Last:                         %"PRIu64" us\n", bench_tsc_to_us(the_stats.last_packet_transmit_to_bf_ts - the_stats.last_packet_receive_net_ts ));
		printf("Last:                         %"PRIu64" cycles\n", (the_stats.last_packet_transmit_to_bf_ts - the_stats.last_packet_receive_net_ts ));
		ele_in_array = the_stats.net_to_bf_i > NUM_OF_RECORDS ? NUM_OF_RECORDS : the_stats.net_to_bf_i;
		printf("Running Average  (of last %d): %"PRIu64" us\n", ele_in_array, bench_tsc_to_us(my_bench_avg(the_stats.from_net_to_bf_diff, ele_in_array)));
		printf("Running Variance (of last %d): %"PRIu64" us\n", ele_in_array, bench_tsc_to_us(my_bench_variance(the_stats.from_net_to_bf_diff, ele_in_array)));
		printf("Running Min      (of last %d): %"PRIu64" us\n", ele_in_array, bench_tsc_to_us(my_bench_min(the_stats.from_net_to_bf_diff, ele_in_array)));
		printf("Running Max      (of last %d): %"PRIu64" us\n", ele_in_array, bench_tsc_to_us(my_bench_max(the_stats.from_net_to_bf_diff, ele_in_array)));
		printf("Data in us:\n");
		for(int i=0; i<ele_in_array; i++){
			printf("%"PRIu64", ", bench_tsc_to_us(the_stats.from_net_to_bf_diff[i]));
		}
		printf("\n");
	}
	printf("-----------\n");
}


