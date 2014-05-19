/* Portmap RPC spec from RFC1057 */

const PMAP_PORT = 111;      /* portmapper port number */

struct mapping {
   uint32_t prog;
   uint32_t vers;
   uint32_t prot;
   uint32_t port;
};

const IPPROTO_TCP = 6;      /* protocol number for TCP/IP */
const IPPROTO_UDP = 17;     /* protocol number for UDP/IP */

struct pmaplist {
   mapping map;
   pmaplist *next;
};

struct call_args {
   uint32_t prog;
   uint32_t vers;
   uint32_t proc;
   opaque args<>;
};

struct call_result {
   uint32_t port;
   opaque res<>;
};

program PMAP_PROG {
   version PMAP_VERS {
      void
      PMAPPROC_NULL(void)         = 0;

      bool
      PMAPPROC_SET(mapping)       = 1;

      bool
      PMAPPROC_UNSET(mapping)     = 2;

      uint32_t
      PMAPPROC_GETPORT(mapping)   = 3;

      pmaplist
      PMAPPROC_DUMP(void)         = 4;

      call_result
      PMAPPROC_CALLIT(call_args)  = 5;
   } = 2;
} = 100000;
