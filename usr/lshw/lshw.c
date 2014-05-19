/*
 * Copyright (c) 2013 ETH Zurich.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <octopus/octopus.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>

#define OPT_HELP             0
#define OPT_LIST_PCI         1
#define OPT_LIST_HW          2
#define OPT_LIST_ALL         3
#define OPT_LIST_PATTERN     4


struct opt_t {
    const char *option;
    const char *pattern;
    const char *help;
    int option_number;
};

/*
 * Program options and help text.
 */
static struct opt_t option[] = {
   { "-pci",  "r'hw\\.pci.'", "List all available PCI devices.", OPT_LIST_PCI },
   { "-hw",   "r'hw\\.'",      "List all found hardware.",   OPT_LIST_HW },
   { "-all",  "r'.*'",        "List all registered types.", OPT_LIST_ALL },
   { "-free", 0,              "Use free search pattern.",   OPT_LIST_PATTERN },
   { "-help", 0,              "Print this help.",             OPT_HELP },
   { 0, 0, 0, 0 }
};

/**
 *
 */
static int exit_help(const char *name)
{
    fprintf(stderr, "Usage: %s <[option] | [pattern]>\n", name);
    fprintf(stderr, "options: \n");

    for (int i = 0; option[i].option != 0; i++)
        fprintf(stderr, "  %s    %s\n", option[i].option, option[i].help);

    exit(1);
}

/**
 *
 */
static void print_pci(const char *data)
{
    int pci, bus, class, device, device_id, function, prog_if, subclass, vendor;
    int i = 0, retval = 0;

    const char *supported_fmt[][2] = {
        { /* PCI */
          "hw.pci.device.%u { "
          "bus: %u, class: %u, device: %u, "
          "device_id: %u, function: %u, prog_if: %u, "
          "subclass: %u, vendor: %u }", "pci"
        },{ /* PCIe */
          "hw.pcie.device.%u { "
          "bus: %u, class: %u, device: %u, "
          "device_id: %u, function: %u, prog_if: %u, "
          "subclass: %u, vendor: %u }", "pcie"
        },{ /* terminator */
           0, 0
        }
    };

    /* parse octopus device listing */
    while (supported_fmt[i][0] != 0 && retval < 9) {
        retval = sscanf(data, supported_fmt[i][0],
                        &pci, &bus, &class, &device, &device_id,
                        &function, &prog_if, &subclass, &vendor);
        if (retval < 9)
            i++;
    }

    if (retval >= 9) {
        printf("hw.%s.device.%u { "
               "bus: %u, class: 0x%x, device: 0x%x, "
               "device_id: 0x%x, function: 0x%x, prog_if: %u, "
               "subclass: 0x%x, vendor: 0x%x }\n",
               supported_fmt[i][1], pci, bus, class, device, device_id,
               function, prog_if, subclass, vendor);
    } else {
        printf("%s\n", data);
    }
}

/**
 *
 */
int main(int argc, char **argv)
{
    const char *pattern;
    char **names;
    errval_t err;
    size_t size;
    iref_t iref;
    int opt = 0;

    if (argc > 1) {
        /* Get selected option */
        for (opt = 0; option[opt].option != 0; opt++) {
            if (strcmp(option[opt].option, argv[1]) == 0)
                break;
        }
    }

    if (option[opt].option_number == OPT_HELP)
        exit_help(argv[0]);

    pattern = option[opt].pattern;

    if (option[opt].option_number == OPT_LIST_PATTERN) {
        if (argc < 3)
            exit_help(argv[0]);

        pattern = argv[2];
    }

    err = nameservice_blocking_lookup("pci_discovery_done", &iref);
    if (err_is_fail(err)) {
        fprintf(stderr,
                "Error: nameservice_blocking_lookup failed for pci_discovery_done.\n");
        exit(err);
    }

    err = oct_init();
    if (err_is_fail(err)) {
        fprintf(stderr, "Error: oct_init failed.\n");
        exit(err);
    }

    err = oct_get_names(&names, &size, pattern);
    if (err) {
        fprintf(stderr, "Error: oct_get_names failed.\n");
        exit(err);
    }

    for (int i = 0; i < size; i++) {
        char *data;

        err = oct_get(&data, names[i]);
        if (err) {
            fprintf(stderr, "Error: oct_get failed.\n");
            exit(err);
        }

        if (option[opt].option_number == OPT_LIST_PCI)
            print_pci(data);
        else
            printf("%s\n", data);

        free(data);
    }

    if (size)
        oct_free_names(names, size);

    return 0;
}
