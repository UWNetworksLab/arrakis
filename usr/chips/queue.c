#include <stdio.h>
#include "queue.h"

errval_t new_ns_reply(struct ns_reply_state** ns, reply_handler_fn s)
{
    *ns = malloc(sizeof(struct ns_reply_state));
    if (*ns == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    (*ns)->cap = NULL_CAP;
    (*ns)->err = ERR_NOTIMP;
    (*ns)->iref = 0;

    (*ns)->rpc_reply = s;
    (*ns)->next = NULL;

    return SYS_ERR_OK;
}


static void send_next(void *arg)
{
    struct nameservice_binding *b = arg;

    struct ns_reply_state* current = dequeue_msg_state(b);

    // If more state in the queue, send them
    if (current) {
    	current->rpc_reply(b, current);
    }
}


void enqueue_msg_state(struct nameservice_binding *b, struct ns_reply_state* st)
{
	if(b->st == NULL) {
        struct waitset *ws = get_default_waitset();
		b->register_send(b, ws, MKCONT(send_next, b));
	}

    struct ns_reply_state** walk = (struct ns_reply_state**) &(b->st);
    for(; *walk != NULL; walk = &(*walk)->next) {
    	// continue
    }
    *walk = st;
    st->next = NULL;
}


struct ns_reply_state* dequeue_msg_state(struct nameservice_binding *b)
{
    struct ns_reply_state* head = b->st;
    b->st = head->next;

    // Reregister for sending, if we need to send more
    if (b->st != NULL) {
        struct waitset *ws = get_default_waitset();
        errval_t err = b->register_send(b, ws, MKCONT(send_next, b));
        assert(err_is_ok(err));
    }

    return head;
}

