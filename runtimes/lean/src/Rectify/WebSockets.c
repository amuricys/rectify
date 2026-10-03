// src/Rectify/WebSockets.c
#include <lean/lean.h>
#include <libwebsockets.h>
#include <string.h>
#include <pthread.h>
#include <stdbool.h>

#define MAX_PAYLOAD 65536
#define MAX_CLIENTS 10
#define MSG_QUEUE_SIZE 64

// Message queue for incoming messages
typedef struct {
    char* messages[MSG_QUEUE_SIZE];
    int head;
    int tail;
    pthread_mutex_t mutex;
} MessageQueue;

// Per-connection data
struct per_session_data {
    struct lws* wsi;
    char* pending_write;
    struct per_session_data* next;
};

// Global state
static struct {
    struct lws_context* context;
    struct per_session_data* clients;
    MessageQueue recv_queue;
    pthread_mutex_t clients_mutex;
    char* broadcast_msg;
    pthread_mutex_t broadcast_mutex;
} g_state = {0};

// Initialize message queue
static void init_queue(MessageQueue* q) {
    memset(q, 0, sizeof(MessageQueue));
    pthread_mutex_init(&q->mutex, NULL);
}

// Enqueue message
static void enqueue_message(const char* msg) {
    pthread_mutex_lock(&g_state.recv_queue.mutex);
    
    int next = (g_state.recv_queue.tail + 1) % MSG_QUEUE_SIZE;
    if (next != g_state.recv_queue.head) {
        g_state.recv_queue.messages[g_state.recv_queue.tail] = strdup(msg);
        g_state.recv_queue.tail = next;
    }
    
    pthread_mutex_unlock(&g_state.recv_queue.mutex);
}

// Dequeue message
static char* dequeue_message() {
    pthread_mutex_lock(&g_state.recv_queue.mutex);
    
    char* msg = NULL;
    if (g_state.recv_queue.head != g_state.recv_queue.tail) {
        msg = g_state.recv_queue.messages[g_state.recv_queue.head];
        g_state.recv_queue.head = (g_state.recv_queue.head + 1) % MSG_QUEUE_SIZE;
    }
    
    pthread_mutex_unlock(&g_state.recv_queue.mutex);
    return msg;
}

// Protocol callback
static int callback_dynamics(struct lws *wsi, enum lws_callback_reasons reason,
                            void *user, void *in, size_t len) {
    struct per_session_data* pss = (struct per_session_data*)user;
    
    switch (reason) {
    case LWS_CALLBACK_ESTABLISHED: {
        printf("[WS] Client connected\n");
        pss->wsi = wsi;
        pss->pending_write = NULL;
        
        // Add to client list
        pthread_mutex_lock(&g_state.clients_mutex);
        pss->next = g_state.clients;
        g_state.clients = pss;
        pthread_mutex_unlock(&g_state.clients_mutex);
        break;
    }
    
    case LWS_CALLBACK_CLOSED: {
        printf("[WS] Client disconnected\n");
        
        // Remove from client list
        pthread_mutex_lock(&g_state.clients_mutex);
        struct per_session_data** pp = &g_state.clients;
        while (*pp) {
            if (*pp == pss) {
                *pp = pss->next;
                break;
            }
            pp = &(*pp)->next;
        }
        pthread_mutex_unlock(&g_state.clients_mutex);
        
        if (pss->pending_write) {
            free(pss->pending_write);
            pss->pending_write = NULL;
        }
        break;
    }
    
    case LWS_CALLBACK_SERVER_WRITEABLE: {
        if (pss->pending_write) {
            unsigned char buf[LWS_PRE + MAX_PAYLOAD];
            int n = strlen(pss->pending_write);
            if (n > MAX_PAYLOAD) n = MAX_PAYLOAD;
            
            memcpy(&buf[LWS_PRE], pss->pending_write, n);
            int written = lws_write(wsi, &buf[LWS_PRE], n, LWS_WRITE_TEXT);
            
            if (written < 0) {
                printf("[WS] Write failed\n");
            }
            
            free(pss->pending_write);
            pss->pending_write = NULL;
        }
        break;
    }
    
    case LWS_CALLBACK_RECEIVE: {
        // Null-terminate the received data
        char* msg = malloc(len + 1);
        memcpy(msg, in, len);
        msg[len] = '\0';
        
        printf("[WS] Received: %s\n", msg);
        enqueue_message(msg);
        free(msg);
        break;
    }
    
    default:
        break;
    }
    
    return 0;
}

static struct lws_protocols protocols[] = {
    {
        "dynamics-protocol",
        callback_dynamics,
        sizeof(struct per_session_data),
        MAX_PAYLOAD,
    },
    { NULL, NULL, 0, 0 }
};

// Initialize WebSocket server
LEAN_EXPORT lean_obj_res lean_ws_init(uint16_t port, lean_obj_arg world) {
    if (g_state.context) {
        return lean_io_result_mk_error(lean_mk_string("WebSocket already initialized"));
    }
    
    // Initialize mutexes and queue
    pthread_mutex_init(&g_state.clients_mutex, NULL);
    pthread_mutex_init(&g_state.broadcast_mutex, NULL);
    init_queue(&g_state.recv_queue);
    
    // Configure server
    struct lws_context_creation_info info;
    memset(&info, 0, sizeof info);
    
    info.port = port;
    info.protocols = protocols;
    info.options = LWS_SERVER_OPTION_ALLOW_NON_SSL_ON_SSL_PORT;
    
    g_state.context = lws_create_context(&info);
    if (!g_state.context) {
        return lean_io_result_mk_error(lean_mk_string("Failed to create WebSocket context"));
    }
    
    printf("[WS] Server started on port %d\n", port);
    return lean_io_result_mk_ok(lean_box(0));
}

// Broadcast message to all clients
LEAN_EXPORT lean_obj_res lean_ws_broadcast(b_lean_obj_arg msg, lean_obj_arg world) {
    if (!g_state.context) {
        return lean_io_result_mk_error(lean_mk_string("WebSocket not initialized"));
    }
    
    const char* msg_str = lean_string_cstr(msg);
    printf("[WS] Broadcasting: %.100s...\n", msg_str);
    
    pthread_mutex_lock(&g_state.clients_mutex);
    struct per_session_data* pss = g_state.clients;
    while (pss) {
        if (pss->pending_write) {
            free(pss->pending_write);
        }
        pss->pending_write = strdup(msg_str);
        lws_callback_on_writable(pss->wsi);
        pss = pss->next;
    }
    pthread_mutex_unlock(&g_state.clients_mutex);
    
    return lean_io_result_mk_ok(lean_box(0));
}

// Service WebSocket (non-blocking)
LEAN_EXPORT lean_obj_res lean_ws_service(uint32_t timeout_ms, lean_obj_arg world) {
    if (!g_state.context) {
        return lean_io_result_mk_error(lean_mk_string("WebSocket not initialized"));
    }
    
    int n = lws_service(g_state.context, timeout_ms);
    if (n < 0) {
        return lean_io_result_mk_error(lean_mk_string("WebSocket service error"));
    }
    
    return lean_io_result_mk_ok(lean_box(0));
}

// Receive message (non-blocking)
LEAN_EXPORT lean_obj_res lean_ws_receive_nonblocking(lean_obj_arg world) {
    if (!g_state.context) {
        return lean_io_result_mk_error(lean_mk_string("WebSocket not initialized"));
    }
    
    char* msg = dequeue_message();
    if (msg) {
        lean_object* lean_msg = lean_mk_string(msg);
        free(msg);
        lean_object* some = lean_alloc_ctor(1, 1, 0);
        lean_ctor_set(some, 0, lean_msg);
        return lean_io_result_mk_ok(some);
    } else {
        lean_object* none = lean_alloc_ctor(0, 0, 0);
        return lean_io_result_mk_ok(none);
    }
}

// Cleanup
LEAN_EXPORT lean_obj_res lean_ws_destroy(lean_obj_arg world) {
    if (g_state.context) {
        // Clean up clients
        pthread_mutex_lock(&g_state.clients_mutex);
        struct per_session_data* pss = g_state.clients;
        while (pss) {
            if (pss->pending_write) {
                free(pss->pending_write);
            }
            pss = pss->next;
        }
        g_state.clients = NULL;
        pthread_mutex_unlock(&g_state.clients_mutex);
        
        // Clean up message queue
        char* msg;
        while ((msg = dequeue_message()) != NULL) {
            free(msg);
        }
        
        // Destroy context
        lws_context_destroy(g_state.context);
        g_state.context = NULL;
        
        // Destroy mutexes
        pthread_mutex_destroy(&g_state.clients_mutex);
        pthread_mutex_destroy(&g_state.broadcast_mutex);
        pthread_mutex_destroy(&g_state.recv_queue.mutex);
    }
    
    printf("[WS] Server destroyed\n");
    return lean_io_result_mk_ok(lean_box(0));
}