
/* ------------------------ Defines ---------------------------*/

/// Node id of a node in an application group
typedef uint32_t route_nodeid_t;

/// ID of an application group
typedef uint32_t route_group_id_t;

/// ID of a multicast group within an application group
typedef uint32_t route_multicast_id_t;

/// ID of a application group broadcast
typedef uint32_t route_group_bcast_id_t;

/// ID of the destination of the message
typedef uint32_t route_destination_id_t;

/// Which set of semantics should the routing library provide
enum route_semantics {
    ROUTE_SEMANTICS_SSF, ///< Reliable and single source FIFO delivery
    ROUTE_SEMANTICS_CO,  ///< Reliable and causal order delivery
    ROUTE_SEMANTICS_TO,  ///< Reliable and total order delivery
};

/* ------------------------ Callbacks ---------------------------*/

/**
 * \brief Called to deliver a message to the application layer
 *
 * \param st         The state user associated with the routing layer
 * \param id         The associated application group
 * \param entry      The entry to which the message was sent.
 * \param sender_id  Id of the sender of the message
 * \param payload    The payload
 */
typedef void (*route_recv_fn)(void *st, route_group_id_t id,
                                  route_destination_id_t entry,
                                  route_nodeid_t sender_id, uint8_t *payload,
                                  size_t length);

/**
 * \brief Called when joining an application group is done
 *
 * \param st         The state user associated with the routing layer
 * \param id         ID of the application group
 */
typedef void (*route_join_group_fn)(void *st, route_group_id_t id);

/**
 * \brief Called when leaving an application group is done
 *
 * \param st         The state user associated with the routing layer
 * \param id         ID of the application group
 */
typedef void (*route_leave_group_fn)(void *st, route_group_id_t id);

/**
 * \brief Called when joining a multicast group is done
 *
 * \param st         The state user associated with the routing layer
 * \param group_id   ID of the application group
 * \param id         The multicast group that was joined
 */
typedef void (*route_join_multicast_group_fn)(void *st,
                          route_group_id_t group_id, route_multicast_id_t id);

/**
 * \brief Called when leave a multicast group is done
 *
 * \param st         The state user associated with the routing layer
 * \param group_id   ID of the application group
 * \param id         The multicast group that was joined
 */
typedef void (*route_leave_multicast_group_fn)(void *st,
                          route_group_id_t group_id, route_multicast_id_t id);

struct route_cb_vtbl {
    route_recv_fn recv;
    route_join_group_fn join_group;
    route_leave_group_fn leave_group;
    route_join_multicast_group_fn join_multicast_group;
    route_leave_multicast_group_fn leave_multicast_group;
};

/**
 * \brief Called when the routing library has initialized
 *
 * \param st   State provided by the user
 */
typedef void (*route_init_fn)(void *st);

/**
 * \brief Called when the previous send finishes
 *
 * \param st         The state user associated with the routing layer
 * \param id         The instance of the routing library
 * \param entry      Entry to which the message was sent
 */
typedef void (*route_cont_fn)(void *st, route_group_id_t id,
                                  route_destination_id_t entry);

/* ------------------------ API ---------------------------*/

/**
 * \brief Initialize the routing library.
 * This must be called on each dispatcher before they can use the routing library
 *
 * \param cb  Callback for when the library is done initializing
 * \param st  User state to associate with the callback function
 */
errval_t route_init(route_init_callback cb, void *st);

/**
 * \brief Create a new application group
 *
 * \param semantics Which semantics the routing library should provide
 * \param id        Return the id of the newly created application group
 *
 * For a group this is called just once.
 * The caller should propagate the id to other dispatcher
 * and call #route_join_group
 */
errval_t route_new_group(enum route_semantics semantics, route_group_id_t *id);

/**
 * \brief Join an application group
 *
 * \param id    Id of the group to join
 * \param st    State to associate with the group
 * \param vtbl  Callback handlers to associate with the group
 *
 * When join is complete #route_join_group_fn will be called.
 */
errval_t route_join_group(route_group_id_t id, void *st,
                          struct route_cb_vtbl vtbl);

/**
 * \brief Leave an application group
 *
 * \param id    Id of the group to leave
 *
 * When leave is complete #route_leave_group_fn will be called.
 */
errval_t route_leave_group(route_group_id_t id);

/**
 * \brief Setup a new multicast group
 *
 * \param group_id  Group id identifies the instance of the routing library
 * \param id    Return the id of the newly created multicast group
 *
 * For a group this is called just once.
 * The caller should propagate the id to other nodes and call
 * #route_join_multicast_group.
 */
errval_t route_new_multicast_group(route_multicast_id_t *id);

/**
 * \brief Join a multicast group
 *
 * \param group_id    Id of the application group
 * \param id          Id of the multicast group to join
 *
 * Join a multicast group within a application group specified by #group_id
 * When join is complete, #route_join_multicast_group_fn will be called.
 */
errval_t route_join_multicast_group(route_group_id_t group_id,
                                    route_multicast_id_t id);

/**
 * \brief Leave a multicast group
 *
 * \param group_id    Id of the application group
 * \param id          Id of the multicast group to leave
 *
 * Leave a multicast group within a application group specified by #group_id
 * When leave is complete, #route_leave_multicast_group_fn will be called.
 */
errval_t route_leave_multicast_group(route_group_id_t group_id,
                                     route_multicast_id_t id);

/**
 * \brief Send a message over the routing layer
 *
 * \param cont     If set, will be called when the send finishes
 * \param id       The application group over which the message should be sent
 * \param dest     The forwarding table entry to which to send the message
 * \param payload  The payload
 */
errval_t route_send(route_cont_fn cont, route_group_id_t id,
                    route_destination_id_t dest,
                    uint8_t *payload, size_t length);

/**
 * \brief Check if can send a message
 *
 * \param id    The application group on which to check
 * \param entry The entry on which to check
 */
bool route_can_send(route_group_id_t id, route_destination_id_t entry);
