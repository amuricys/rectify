// src/Rectify/WebSockets.c
#include <lean/lean.h>
#include <libwebsockets.h>
#include <string.h>
#include <signal.h>
#include <stdio.h>

static struct lws_context *context = NULL;
static int interrupted = 0;
static char* pending_message = NULL;

// Protocol definition
static int callback_dynamics(struct lws *wsi, enum lws_callback_reasons reason,
                            void *user, void *in, size_t len) {
    switch (reason) {
    case LWS_CALLBACK_ESTABLISHED:
        printf("Connection established\n");
        break;
        
    case LWS_CALLBACK_SERVER_WRITEABLE:
        if (pending_message) {
            unsigned char buf[LWS_PRE + 2048];
            int n = strlen(pending_message);
            memcpy(&buf[LWS_PRE], pending_message, n);
            
            lws_write(wsi, &buf[LWS_PRE], n, LWS_WRITE_TEXT);
            
            free(pending_message);
            pending_message = NULL;
        }
        break;
        
    case LWS_CALLBACK_RECEIVE:
        printf("Received: %.*s\n", (int)len, (char *)in);
        // Echo back or handle commands
        break;
        
    default:
        break;
    }
    
    return 0;
}

static struct lws_protocols protocols[] = {
    {
        "dynamics-protocol",
        callback_dynamics,
        0,
        1024,
    },
    { NULL, NULL, 0, 0 }
};

// Initialize WebSocket context
LEAN_EXPORT lean_obj_res lean_ws_init(uint16_t port, lean_obj_arg world) {
    struct lws_context_creation_info info;
    memset(&info, 0, sizeof info);
    
    info.port = port;
    info.protocols = protocols;
    
    context = lws_create_context(&info);
    if (!context) {
        return lean_io_result_mk_error(lean_mk_string("Failed to create WebSocket context"));
    }
    
    printf("WebSocket server started on port %d\n", port);
    return lean_io_result_mk_ok(lean_box(0));
}

// Send a message to all connected clients
LEAN_EXPORT lean_obj_res lean_ws_broadcast(b_lean_obj_arg msg, lean_obj_arg world) {
    if (!context) {
        return lean_io_result_mk_error(lean_mk_string("WebSocket not initialized"));
    }
    
    // Store message to send
    const char* msg_str = lean_string_cstr(msg);
    if (pending_message) free(pending_message);
    pending_message = strdup(msg_str);
    
    printf("Broadcasting: %s\n", msg_str);
    
    // Request writeable callback for all connections
    lws_callback_on_writable_all_protocol(context, &protocols[0]);
    
    return lean_io_result_mk_ok(lean_box(0));
}

// Service the WebSocket (needs to be called periodically)
LEAN_EXPORT lean_obj_res lean_ws_service(uint32_t timeout_ms, lean_obj_arg world) {
    if (!context) {
        return lean_io_result_mk_error(lean_mk_string("WebSocket not initialized"));
    }
    
    int n = lws_service(context, timeout_ms);
    if (n < 0) {
        return lean_io_result_mk_error(lean_mk_string("WebSocket service error"));
    }
    
    return lean_io_result_mk_ok(lean_box(0));
}

// Cleanup
LEAN_EXPORT lean_obj_res lean_ws_destroy(lean_obj_arg world) {
    if (context) {
        lws_context_destroy(context);
        context = NULL;
    }
    if (pending_message) {
        free(pending_message);
        pending_message = NULL;
    }
    printf("WebSocket server destroyed\n");
    return lean_io_result_mk_ok(lean_box(0));
}