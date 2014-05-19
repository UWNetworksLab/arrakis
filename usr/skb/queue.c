#include <stdio.h>

#include <include/queue.h>
#include <include/skb_debug.h>

static void skb_send_next(void *arg)
{
    struct skb_binding *b = arg;

    struct skb_reply_state* current = dequeue_reply_state(b);

    // If more state in the queue, send them
    if (current) {
    	current->rpc_reply(b, current);
    }
}


void enqueue_reply_state(struct skb_binding *b, struct skb_reply_state* st)
{
	if(b->st == NULL) {
        struct waitset *ws = get_default_waitset();
		b->register_send(b, ws, MKCONT(skb_send_next, b));
	}

    struct skb_reply_state** walk = (struct skb_reply_state**) &(b->st);
    for(; *walk != NULL; walk = &(*walk)->next) {
    	// continue
    }
    *walk = st;
    st->next = NULL;
}


struct skb_reply_state* dequeue_reply_state(struct skb_binding *b)
{
    struct skb_reply_state* head = b->st;
    b->st = head->next;

    // Reregister for sending, if we need to send more
    if (b->st != NULL) {
        struct waitset *ws = get_default_waitset();
        errval_t err = b->register_send(b, ws, MKCONT(skb_send_next, b));
        assert(err_is_ok(err));
    }

    return head;
}

