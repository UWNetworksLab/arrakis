#include <queue.h>

static void send_next(void *arg)
{
    struct monitor_binding *b = arg;

    struct mon_msg_state* current = dequeue_msg_state(b);

    // If more state in the queue, send it
    if (current) {
    	current->send(b, current);
    }
}

void enqueue_msg_state(struct monitor_binding *b, struct mon_msg_state *st)
{
	if(b->st == NULL) {
        struct waitset *ws = get_default_waitset();
		b->register_send(b, ws, MKCONT(send_next, b));
	}

    struct mon_msg_state** walk = (struct mon_msg_state**) &(b->st);
    for(; *walk != NULL; walk = &(*walk)->next) {
    	// continue
    }
    *walk = st;
    st->next = NULL;
}

struct mon_msg_state* dequeue_msg_state(struct monitor_binding *b)
{
    struct mon_msg_state *head = b->st;
    b->st = head->next;

    // Reregister for sending, if we need to send more
    if (b->st != NULL) {
        struct waitset *ws = get_default_waitset();
        errval_t err = b->register_send(b, ws, MKCONT(send_next, b));
        assert(err_is_ok(err));
    }

    return head;
}

