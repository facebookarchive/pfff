/* $Id: netsys_c_win32.c 1505 2010-12-15 19:15:22Z gerd $ */

/* Note: need -lws2_32 to link this file, e.g. */
/* ocamlmktop -o otop -I . -cclib -lws2_32 unix.cma netsys.cma */

#include "netsys_c.h"
#include <stdio.h>
#include <assert.h>

static int debug = 0;

#ifdef _WIN32

#include "unixsupport_w32.c"
#include <wincrypt.h>
#include <stdarg.h>


static void dprintf(const char *fmt, ...) {
    va_list list;
    if (debug) {
	va_start(list, fmt);
	vfprintf(stderr, fmt, list);
	va_end(list);
	fflush(stderr);
    }
}

static HCRYPTPROV crypt_provider;
static int        crypt_provider_init = 0;
#endif


CAMLprim value netsys_win32_set_debug (value flag) {
    debug = Bool_val(flag);
    return Val_unit;
}


CAMLprim value netsys_fill_random (value s) {
#ifdef _WIN32
    if (!crypt_provider_init) {
	if (!CryptAcquireContext(&crypt_provider,
				 NULL,
				 NULL,
				 PROV_RSA_FULL,
				 CRYPT_VERIFYCONTEXT)) {
	    win32_maperr(GetLastError());
	    uerror("netsys_fill_random/CryptAcquireContext", Nothing);
	};
	crypt_provider_init = 1;
    }
    if(!CryptGenRandom(crypt_provider,
		       string_length(s),
		       String_val(s))) {
	win32_maperr(GetLastError());
	uerror("netsys_fill_random/CryptGenRandom", Nothing);
    };
    return Val_unit;
#else
    invalid_argument("netsys_fill_random");
#endif
}


/* The implementation in Unix is problematic because the handle is
   duplicated!
*/
CAMLprim value netsys_modify_close_on_exec (value fd, value new_val) {
#ifdef _WIN32
    int flag;
    flag = Bool_val(new_val);
    if (!SetHandleInformation(Handle_val(fd), HANDLE_FLAG_INHERIT,
			      flag ? 0 : HANDLE_FLAG_INHERIT)) {
	win32_maperr(GetLastError());
	uerror("netsys_modify_close_on_exec/SetHandleInformation", Nothing);
    }
    return Val_unit;
#else
    invalid_argument("netsys_modify_close_on_exec");
#endif
}


CAMLprim value netsys_test_close_on_exec (value fd) {
#ifdef _WIN32
    DWORD flags;
    int r;
    dprintf("netsys_test_close_on_exec fd=%u\n", Handle_val(fd));
    if (!GetHandleInformation(Handle_val(fd), &flags)) {
	win32_maperr(GetLastError());
	uerror("netsys_test_close_on_exec/GetHandleInformation", Nothing);
    }
    r = (flags & HANDLE_FLAG_INHERIT) != 0;
    dprintf("netsys_test_close_on_exec fd=%u result=%s\n", 
	    Handle_val(fd), r ? "true" : "false");
    return Val_bool(r);
#else
    invalid_argument("netsys_test_close_on_exec");
#endif
}


CAMLprim value netsys_is_crt_fd (value fd, value crt_fd) {
#ifdef _WIN32
    int fd1;
    fd1 = CRT_fd_val(fd);
    if (fd1 == NO_CRT_FD)
	return Val_bool(0);
    else
	return Val_bool(fd1 == Int_val(crt_fd));
#else
    invalid_argument("netsys_is_crt_fd");
#endif
}



#ifdef _WIN32
/* Additions to the socket/handle API for Win32: */

static struct custom_operations event_ops = {
    "",
    custom_finalize_default,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default
};


struct event {
    HANDLE ev;
    int    mask;
    HANDLE ev_proxy;    /* A copy of ev, to be used for the proxy descriptor */
    int    auto_close;  /* Whether to auto-close ev_proxy. ev is always 
			   auto-closed 
			*/
};



#define event_val(v) ((struct event *) (Data_custom_val(v)))


static value alloc_event(HANDLE e) {
    value r;
    struct event *e0;
    int flag;
    HANDLE e_proxy;

    flag = DuplicateHandle(GetCurrentProcess(),
			   e, 
			   GetCurrentProcess(),
			   &e_proxy, 
			   0,
			   FALSE,
			   DUPLICATE_SAME_ACCESS);
    if (!flag) {
	win32_maperr(GetLastError());
	uerror("alloc_event/DuplicateHandle", Nothing);
    };

    r = caml_alloc_custom(&event_ops, sizeof(struct event), 1, 0);
    e0 = event_val(r);

    e0->ev = e;
    e0->mask = 0;
    e0->ev_proxy = e_proxy;
    e0->auto_close = 1;

    return r;
}
#endif


CAMLprim value netsys_create_event(value dummy) {
#ifdef _WIN32
    HANDLE e, e_proxy;
    value r;
    struct event *e0;
    int flag;

    e = CreateEvent(NULL, 1, 0, NULL);
    if (e == NULL) {
	win32_maperr(GetLastError());
	uerror("netsys_create_event/CreateEvent", Nothing);
    };

    return alloc_event(e);
#else
    invalid_argument("netsys_create_event");
#endif
}

CAMLprim value netsys_close_event(value ev) {
#ifdef _WIN32
    struct event *e;

    e = event_val(ev);

    dprintf("EVENT netsys_close_event handle=%u proxy=%u auto_close=%d\n", 
	    e->ev,
	    e->ev_proxy,
	    e->auto_close);

    CloseHandle(e->ev);  /* ignore errors - this is a finaliser! */
    if (e->auto_close) {
	CloseHandle(e->ev_proxy);  /* ignore errors - this is a finaliser! */
    };

    return Val_unit;
#else
    invalid_argument("netsys_close_event");
#endif
}


CAMLprim value netsys_set_auto_close_event_proxy(value ev, value flag) {
#ifdef _WIN32
    struct event *e;

    e = event_val(ev);
    e->auto_close = Bool_val(flag);
    return Val_unit;
#else
    invalid_argument("netsys_set_auto_close_event_proxy");
#endif
}


CAMLprim value netsys_set_event(value ev) {
#ifdef _WIN32
    struct event *e;

    e = event_val(ev);
    if (!SetEvent(e->ev)) {
	win32_maperr(GetLastError());
	uerror("netsys_set_event/SetEvent", Nothing);
    }

    return Val_unit;
#else
    invalid_argument("netsys_set_event");
#endif
}

CAMLprim value netsys_reset_event(value ev) {
#ifdef _WIN32
    struct event *e;

    e = event_val(ev);
    if (!ResetEvent(e->ev)) {
	win32_maperr(GetLastError());
	uerror(" netsys_reset_event/ResetEvent", Nothing);
    }

    return Val_unit;
#else
    invalid_argument("netsys_reset_event");
#endif
}


CAMLprim value netsys_test_event(value ev) {
#ifdef _WIN32
    struct event *e;
    DWORD n;
    int signaled;

    e = event_val(ev);
    n = WaitForSingleObject(e->ev, 0);
    if (n == WAIT_FAILED) {
	win32_maperr(GetLastError());
	uerror("netsys_test_event/WaitForSingleObject", Nothing);
    };

    return Val_bool(n == WAIT_OBJECT_0);
#else
    invalid_argument("netsys_test_event");
#endif
}


CAMLprim value netsys_event_wait(value ev, value tmo) {
#ifdef _WIN32
    struct event *e;
    DWORD n;
    DWORD wtmo;  /* unsigned! */
    DWORD err;
    int signaled;

    wtmo = INFINITE;
    if (Long_val(tmo) >= 0) {
	wtmo = Long_val(tmo);
    };

    e = event_val(ev);
    enter_blocking_section();
    n = WaitForSingleObject(e->ev, wtmo);
    err = GetLastError();
    leave_blocking_section();
    if (n == WAIT_FAILED) {
	win32_maperr(err);
	uerror("netsys_event_wait/WaitForSingleObject", Nothing);
    };

    return Val_bool(n == WAIT_OBJECT_0);
#else
    invalid_argument("netsys_event_wait");
#endif
}


CAMLprim value netsys_event_descr(value ev) {
#ifdef _WIN32
    struct event *e;
    e = event_val(ev);
    return netsysw32_win_alloc_handle(e->ev_proxy);
#else
    invalid_argument("netsys_event_descr");
#endif
}


CAMLprim value netsys_wsa_event_select(value ev, value fdv, value evmaskv) {
#ifdef _WIN32
    struct event *e;
    SOCKET s;
    int m;
    long m_win32;

    e = event_val(ev);
    s = Socket_val(fdv);
    m = Int_val(evmaskv);

    e->mask = m & (CONST_POLLIN | CONST_POLLOUT | CONST_POLLPRI);

    m_win32 = 0;
    if ((m & CONST_POLLIN) != 0)  m_win32 |= FD_READ | FD_ACCEPT | FD_CLOSE;
    if ((m & CONST_POLLOUT) != 0) m_win32 |= FD_WRITE | FD_CONNECT | FD_CLOSE;
    if ((m & CONST_POLLPRI) != 0) m_win32 |= FD_OOB;

    if (WSAEventSelect(s, e->ev, m_win32) != 0) {
	win32_maperr(WSAGetLastError());
	uerror("netsys_wsa_event_select/WSAEventSelect", Nothing);
    }

    return Val_unit;
#else
    invalid_argument("netsys_wsa_event_select");
#endif
}

CAMLprim value netsys_wsa_maximum_wait_events(value dummy) {
#ifdef _WIN32
    return Val_int(WSA_MAXIMUM_WAIT_EVENTS);
#else
    invalid_argument("netsys_wsa_maximum_wait_events");
#endif
}

CAMLprim value netsys_wsa_wait_for_multiple_events(value fdarray, value tmov) {
#ifdef _WIN32
    WSAEVENT earray[WSA_MAXIMUM_WAIT_EVENTS];
    struct event *e;
    long  tmo0;
    DWORD tmo;   /* note: DWORD is unsigned */
    int k,n;
    DWORD r;
    value rv;
    DWORD err;

    tmo0 = Long_val(tmov);
    n = Wosize_val(fdarray);

    dprintf("WSAWaitForMultipleEvents prep n=%d tmo=%d\n", n, tmo0);

    if (n > WSA_MAXIMUM_WAIT_EVENTS) {
	win32_maperr(EINVAL);
	uerror("netsys_wsa_wait_for_multiple_events", Nothing);
    };

    for (k=0; k < n; k++) {
	e = event_val(Field(fdarray,k));
	earray[k] = e->ev;
	dprintf("WSAWaitForMultipleEvents prep evhandle=%u evproxy=%u\n", 
		e->ev, e->ev_proxy);
    }

    if (n == 0) {
	if (tmo >= 0) 
	    tmo = tmo0;
	else
	    tmo = INFINITE;
	enter_blocking_section();
	r = SleepEx(tmo, 1);
	leave_blocking_section();
	
	if (r == WAIT_IO_COMPLETION) {
	    win32_maperr(EINTR);
	    uerror("netsys_wsa_wait_for_multiple_events/SleepEx", Nothing);
	}

	return Val_int(0);    /* None */
    }
    else {
	if (tmo >= 0) 
	    tmo = tmo0;
	else
	    tmo = INFINITE;
	dprintf("WSAWaitForMultipleEvents start tmo=%u\n", tmo);
	enter_blocking_section();
	r = WSAWaitForMultipleEvents(n, earray, 0, tmo, 1);
	err = WSAGetLastError();
	leave_blocking_section();
	dprintf("WSAWaitForMultipleEvents end code=%u\n", r);
    
	if (r == WSA_WAIT_FAILED) {
	    dprintf("WSAWaitForMultipleEvents error=%u\n", err);
	    /* Sometimes we get err==0. Handle it like timeout: */
	    if (err == 0) 
		return Val_int(0);
	    win32_maperr(err);
	    uerror("netsys_wsa_wait_for_multiple_events/WSAWaitForMultipleEvents", Nothing);
	}
	
	if (r == WSA_WAIT_TIMEOUT)
	    return Val_int(0);    /* None */
	
	if (r == WSA_WAIT_IO_COMPLETION) {
	    dprintf("WSAWaitForMultipleEvents error=EINTR\n");
	    win32_maperr(EINTR);
	    uerror("netsys_wsa_wait_for_multiple_events/WSAWaitForMultipleEvents", Nothing);
	}
	
	if (r >= WSA_WAIT_EVENT_0 && r < WSA_WAIT_EVENT_0 + n) {
	    rv = caml_alloc_tuple(1);
	    Store_field(rv, 0, Val_long(r - WSA_WAIT_EVENT_0));
	    return rv;
	}
    }
    
    invalid_argument("netsys_wsa_wait_for_multiple_events: bad return value from Win32");
#else
    invalid_argument("netsys_wsa_wait_for_multiple_events");
#endif
}

CAMLprim value netsys_wsa_enum_network_events(value fdv, value ev) {
#ifdef _WIN32
    struct event *e;
    SOCKET s;
    WSANETWORKEVENTS ne;
    int r;

    e = event_val(ev);
    s = Socket_val(fdv);

    if (WSAEnumNetworkEvents(s, e->ev, &ne) != 0) {
	win32_maperr(WSAGetLastError());
	uerror("netsys_wsa_enum_network_events/WSAEnumNetworkEvents", Nothing);
    }

    /* printf("NetworkEvents=%ld\n", ne.lNetworkEvents); */

    r = 0;
    if ((ne.lNetworkEvents & FD_CONNECT) != 0) {
	r |= CONST_POLLOUT;
	if (ne.iErrorCode[FD_CONNECT_BIT] != 0) r |= CONST_POLLERR;
    };
    if ((ne.lNetworkEvents & FD_CLOSE) != 0) {
	r |= CONST_POLLIN;
	if (ne.iErrorCode[FD_CLOSE_BIT] != 0) {
	    if (ne.iErrorCode[FD_CLOSE_BIT] == WSAECONNRESET)
		r |= CONST_POLLHUP;
	    else
		r |= CONST_POLLERR;
	}
    };
    if ((ne.lNetworkEvents & FD_ACCEPT) != 0) {
	r |= CONST_POLLIN;
	if (ne.iErrorCode[FD_ACCEPT_BIT] != 0) r |= CONST_POLLERR;
    };
    if ((ne.lNetworkEvents & FD_READ) != 0) {
	r |= CONST_POLLIN;
	if (ne.iErrorCode[FD_READ_BIT] != 0) r |= CONST_POLLERR;
    };
    if ((ne.lNetworkEvents & FD_WRITE) != 0) {
	r |= CONST_POLLOUT;
	if (ne.iErrorCode[FD_WRITE_BIT] != 0) r |= CONST_POLLERR;
    };
    if ((ne.lNetworkEvents & FD_OOB) != 0) {
	r |= CONST_POLLPRI;
	if (ne.iErrorCode[FD_OOB_BIT] != 0) r |= CONST_POLLERR;
    };

    /* printf("r=%d\n", r); */

    r &= (e->mask | CONST_POLLHUP | CONST_POLLERR);

    /* printf("e.mask=%d\n", e.mask); fflush(stdout); */

    return Val_int(r);
#else
    invalid_argument("netsys_wsa_enum_network_events");
#endif
}

/* CHECK:
   - maybe we have to make the FD_CLOSE event last forever (it is not
     level-triggered)
*/

#ifdef _WIN32
#define PIPE_HELPER_BUF_SIZE 1024

#define PIPE_DEAF 0
#define PIPE_LISTENING 1
#define PIPE_CONNECTED 2
#define PIPE_DOWN 3

struct pipe_helper {
    HANDLE pipe_handle;
    int    pipe_is_open;          /* handle is open */
    int    pipe_is_server;
    int    pipe_conn_state;
    int    pipe_error_rd;         /* recorded async error */
    int    pipe_error_wr;         /* recorded async error */
    int    pipe_mode_rd;          /* reads allowed */
    int    pipe_mode_wr;          /* write allowed */
    HANDLE pipe_rd_ev;
    HANDLE pipe_wr_ev;
    HANDLE pipe_cn_ev;
    LPOVERLAPPED pipe_rd_ovrlp; /* overlappped structure for reads */
    LPOVERLAPPED pipe_wr_ovrlp; /* overlappped structure for writes */
    LPOVERLAPPED pipe_cn_ovrlp; /* overlappped structure for connects */
    int    pipe_rd_ovrlp_started; /* an overlapping read has been started */
    int    pipe_wr_ovrlp_started; /* an overlapping write has been started */
    int    pipe_cn_ovrlp_started; /* an overlapping connect has been started */
    char   pipe_rd_buf[PIPE_HELPER_BUF_SIZE];
    int    pipe_rd_buf_size;
    int    pipe_rd_eof;
    char   pipe_wr_buf[PIPE_HELPER_BUF_SIZE];
    int    pipe_wr_buf_size;
    HANDLE pipe_descr;
    int    pipe_descr_auto_close;
    HANDLE pipe_signal;
};

#define pipe_helper_ptr_val(v) ((struct pipe_helper **) (Data_custom_val(v)))

static struct custom_operations pipe_helper_ops = {
    "",
    custom_finalize_default,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default
};


/* It is allowed to pass INVALID_HANDLE_VALUE for cn_ev */

static struct pipe_helper * alloc_pipe_helper (HANDLE h, HANDLE cn_ev) {
    struct pipe_helper *ph;
    HANDLE rd_ev;
    HANDLE wr_ev;
    HANDLE pd;
    OVERLAPPED *rd_ovrlp;
    OVERLAPPED *wr_ovrlp;
    OVERLAPPED *cn_ovrlp;

    /* CHECK: error handling */

    rd_ev = CreateEvent(NULL, 1, 0, NULL);
    if (rd_ev == NULL) {
	win32_maperr(GetLastError());
	uerror("alloc_pipe_helper/CreateEvent", Nothing);
    };

    wr_ev = CreateEvent(NULL, 1, 0, NULL);
    if (wr_ev == NULL) {
	win32_maperr(GetLastError());
	uerror("alloc_pipe_helper/CreateEvent", Nothing);
    };

    pd = CreateEvent(NULL, 1, 0, NULL);
    if (pd == NULL) {
	win32_maperr(GetLastError());
	uerror("alloc_pipe_helper/CreateEvent", Nothing);
    };

    rd_ovrlp = stat_alloc(sizeof(OVERLAPPED));
    ZeroMemory(rd_ovrlp, sizeof(OVERLAPPED));
    rd_ovrlp->hEvent = rd_ev;

    wr_ovrlp = stat_alloc(sizeof(OVERLAPPED));
    ZeroMemory(wr_ovrlp, sizeof(OVERLAPPED));
    wr_ovrlp->hEvent = wr_ev;

    cn_ovrlp = stat_alloc(sizeof(OVERLAPPED));
    ZeroMemory(cn_ovrlp, sizeof(OVERLAPPED));
    if (cn_ev != INVALID_HANDLE_VALUE)
	cn_ovrlp->hEvent = cn_ev;

    ph = stat_alloc(sizeof(struct pipe_helper));
    ph->pipe_handle = h;
    ph->pipe_is_open = 1;
    ph->pipe_is_server = 0;
    ph->pipe_conn_state = PIPE_DEAF;
    ph->pipe_error_rd = 0;
    ph->pipe_error_wr = 0;
    ph->pipe_mode_rd = 0;
    ph->pipe_mode_wr = 0;
    ph->pipe_rd_ev = rd_ev;
    ph->pipe_wr_ev = wr_ev;
    ph->pipe_cn_ev = cn_ev;
    ph->pipe_rd_ovrlp = rd_ovrlp;
    ph->pipe_wr_ovrlp = wr_ovrlp;
    ph->pipe_cn_ovrlp = cn_ovrlp;
    ph->pipe_rd_ovrlp_started = 0;
    ph->pipe_wr_ovrlp_started = 0;
    ph->pipe_cn_ovrlp_started = 0;
    ph->pipe_rd_buf_size = 0;
    ph->pipe_rd_eof = 0;
    ph->pipe_wr_buf_size = 0;
    ph->pipe_signal = NULL;
    ph->pipe_descr = pd;
    ph->pipe_descr_auto_close = 1;

    return ph;
}


static void free_pipe_helper(struct pipe_helper *ph) {
    dprintf("PIPE free_pipe_helper auto_close=%d\n",
	    ph->pipe_descr_auto_close);
    if (ph->pipe_is_open)
	CloseHandle(ph->pipe_handle);
    if (ph->pipe_descr_auto_close)
	CloseHandle(ph->pipe_descr);
    CloseHandle(ph->pipe_rd_ev);
    CloseHandle(ph->pipe_wr_ev);
    if (ph->pipe_cn_ev != INVALID_HANDLE_VALUE)
	CloseHandle(ph->pipe_cn_ev);
    /* do nothing about pipe_signal */
    stat_free(ph->pipe_rd_ovrlp);
    stat_free(ph->pipe_wr_ovrlp);
    stat_free(ph->pipe_cn_ovrlp);
    stat_free(ph);
}


static void start_reading(struct pipe_helper *ph) {
    int flag;
    DWORD err;
    DWORD n;

    dprintf("PIPE start_reading %u\n",
	    ph->pipe_handle);

    flag = ReadFile(ph->pipe_handle,
		    ph->pipe_rd_buf,
		    PIPE_HELPER_BUF_SIZE,
		    &n,
		    ph->pipe_rd_ovrlp);
    ph->pipe_rd_buf_size = 0;

    if (flag) {
	/* should not happen, but we handle it */
	ph->pipe_rd_buf_size = n;
	if (n == 0) ph->pipe_rd_eof = 1;
	dprintf("PIPE start_reading %u success n=%u\n",
		ph->pipe_handle, n);
    } 
    else {
	err = GetLastError();
	if (err == ERROR_IO_PENDING) {
	    ph->pipe_rd_ovrlp_started = 1;
	    dprintf("PIPE start_reading %u pending\n",
		    ph->pipe_handle);
	}
	else if (err == ERROR_HANDLE_EOF) {
	    ph->pipe_rd_eof = 1;
	    dprintf("PIPE start_reading %u error_handle_eof\n",
		    ph->pipe_handle);
	}
	else {
	    ph->pipe_error_rd = err;
	    dprintf("PIPE start_reading %u error code=%u\n",
		    ph->pipe_handle, err);
	}
    }
}


static void start_writing(struct pipe_helper *ph) {
    int flag;
    DWORD err;

    dprintf("PIPE start_writing %u\n",
	    ph->pipe_handle);

    flag = WriteFile(ph->pipe_handle,
		     ph->pipe_wr_buf,
		     ph->pipe_wr_buf_size,
		     NULL,
		     ph->pipe_wr_ovrlp);
    if (flag) {
	/* should not happen, but we handle it */
	ph->pipe_wr_buf_size = 0;
	dprintf("PIPE start_writing %u success\n",
		ph->pipe_handle);
    } 
    else {
	err = GetLastError();
	if (err == ERROR_IO_PENDING) {
	    ph->pipe_wr_ovrlp_started = 1;
	    dprintf("PIPE start_writing %u pending\n",
		    ph->pipe_handle);
	}
	else {
	    ph->pipe_error_wr = err;
	    dprintf("PIPE start_writing %u error code=%u\n",
		    ph->pipe_handle, err);
	}
    }
}


static void check_for_pending_operations(struct pipe_helper *ph) {
    int flag;
    DWORD n;
    DWORD err;
    dprintf("PIPE check_for_pending %u\n",
	    ph->pipe_handle);

    if (ph->pipe_rd_ovrlp_started) {
	flag = GetOverlappedResult(ph->pipe_handle,
				   ph->pipe_rd_ovrlp,
				   &n,
				   0);
	if (flag) {
	    /* operation is done */
	    dprintf("PIPE check_for_pending %u rd success n=%u\n",
		    ph->pipe_handle, n);
	    ph->pipe_rd_buf_size = n;
	    ph->pipe_rd_ovrlp_started = 0;
	}
	else {
	    err = GetLastError();
	    if (err == ERROR_HANDLE_EOF || err == ERROR_BROKEN_PIPE || 
		err == ERROR_NO_DATA
	       ) {
		dprintf("PIPE check_for_pending %u rd eof code=%u\n",
			ph->pipe_handle, err);
		ph->pipe_rd_buf_size = 0;
		ph->pipe_rd_eof = 1;
		ph->pipe_rd_ovrlp_started = 0;
	    } else if (err != ERROR_IO_INCOMPLETE) {
		ph->pipe_error_rd = err;  /* is reported by next pipe_read */
		ph->pipe_rd_ovrlp_started = 0;
		dprintf("PIPE check_for_pending %u rd error code=%u\n",
			ph->pipe_handle, err);
	    } else {
		dprintf("PIPE check_for_pending %u rd pending\n",
			ph->pipe_handle);
	    }
	}
    };
    if (ph->pipe_wr_ovrlp_started) {
	flag = GetOverlappedResult(ph->pipe_handle,
				   ph->pipe_wr_ovrlp,
				   &n,
				   0);
	if (flag) {
	    /* operation is done */
	    dprintf("PIPE check_for_pending %u wr success n=%u\n",
		    ph->pipe_handle, n);
	    /* We assume that always the whole buffer is written! */
	    ph->pipe_wr_buf_size = 0;
	    ph->pipe_wr_ovrlp_started = 0;
	}
	else {
	    err = GetLastError();
	    if (err != ERROR_IO_INCOMPLETE) {
		ph->pipe_error_wr = err;  /* is reported by next pipe_write */
		ph->pipe_wr_ovrlp_started = 0;
		dprintf("PIPE check_for_pending %u wr error code=%u\n",
			ph->pipe_handle, err);
	    } else {
		dprintf("PIPE check_for_pending %u wr pending\n",
			ph->pipe_handle);
	    }
	}
    };
    if (ph->pipe_cn_ovrlp_started) {
	flag = GetOverlappedResult(ph->pipe_handle,
				   ph->pipe_cn_ovrlp,
				   &n,
				   0);
	if (flag) {
	    /* operation is done */
	    dprintf("PIPE check_for_pending %u cn success n=%u\n",
		    ph->pipe_handle, n);
	    ph->pipe_conn_state = PIPE_CONNECTED;
	    if (ph->pipe_mode_rd)
		start_reading(ph);
	    if (ph->pipe_mode_wr)
		SetEvent(ph->pipe_wr_ev);
	    ph->pipe_cn_ovrlp_started = 0;
	}
	else {
	    err = GetLastError();
	    if (err != ERROR_IO_INCOMPLETE) {
		ph->pipe_error_wr = err;  /* is reported by next pipe_write */
		ph->pipe_error_rd = err;  /* also report via pipe_read */
		ph->pipe_cn_ovrlp_started = 0;
		dprintf("PIPE check_for_pending %u cn error code=%u\n",
			ph->pipe_handle, err);
		/* Also an error counts as "connected"... */
		ph->pipe_conn_state = PIPE_CONNECTED;
		if (ph->pipe_mode_rd)
		    SetEvent(ph->pipe_rd_ev);
		if (ph->pipe_mode_wr)
		    SetEvent(ph->pipe_wr_ev);
	    } else {
		dprintf("PIPE check_for_pending %u cn pending\n",
			ph->pipe_handle);
	    }
	}
    };
}
#endif


#ifdef _WIN32
static PSID world_sid = NULL;   /* all logged-in users */
static PSID network_sid = NULL; /* all accesses coming over the network */


void setup_sid(void) {
    SID_IDENTIFIER_AUTHORITY world_sid_auth = {SECURITY_WORLD_SID_AUTHORITY};
    SID_IDENTIFIER_AUTHORITY nt_sid_auth = {SECURITY_NT_AUTHORITY};
    BOOL e;

    // set up network_sid and world_sid:
    if (world_sid == NULL) {
	e = AllocateAndInitializeSid(&world_sid_auth, 1,
				     SECURITY_WORLD_RID,
				     0, 0, 0, 0, 0, 0, 0,
				     &world_sid);
	if (e == 0) {
	    win32_maperr(GetLastError());
	    uerror("setup_sid/AllocateAndInitializeSid", Nothing);
	};
    };

    if (network_sid == NULL) {
	e = AllocateAndInitializeSid(&nt_sid_auth, 1,
				     SECURITY_NETWORK_RID,
				     0, 0, 0, 0, 0, 0, 0,
				     &network_sid);
	if (e == 0) {
	    win32_maperr(GetLastError());
	    uerror("setup_sid/AllocateAndInitializeSid", Nothing);
	};
    }
}
#endif


CAMLprim value netsys_create_local_named_pipe(value name, value mode, 
					      value nv, value cn_ev_v,
					      value firstv
					      )
{
#ifdef _WIN32
    HANDLE h;
    DWORD omode;
    int mode_rd, mode_wr, first;
    DWORD n;
    struct pipe_helper *ph;
    value r;
    HANDLE cn_ev;

    omode = 0;
    mode_rd = 0;
    mode_wr = 0;
    switch (Int_val(mode)) {
    case 0: /* Pipe_in */
	omode = PIPE_ACCESS_INBOUND; mode_rd = 1; break;
    case 1: /* Pipe_out */
	omode = PIPE_ACCESS_OUTBOUND; mode_wr = 1; break;
    case 2: /* Pipe_duplex */
	omode = PIPE_ACCESS_DUPLEX; mode_rd = 1; mode_wr = 1; break;
    };

    n = Int_val(nv);
    if (n <=0 || n > PIPE_UNLIMITED_INSTANCES) n = PIPE_UNLIMITED_INSTANCES;

    cn_ev = event_val(cn_ev_v)->ev;
    first = Bool_val(firstv);
    setup_sid();
    
    h = CreateNamedPipe(String_val(name),
			omode | WRITE_DAC | FILE_FLAG_OVERLAPPED | 
			  (first ? FILE_FLAG_FIRST_PIPE_INSTANCE : 0),
			PIPE_TYPE_BYTE | PIPE_READMODE_BYTE | PIPE_WAIT,
			n,
			PIPE_HELPER_BUF_SIZE,
			PIPE_HELPER_BUF_SIZE,
			0,
			NULL);
    if ( h == INVALID_HANDLE_VALUE ) {
	win32_maperr(GetLastError());
	uerror("netsys_create_local_named_pipe/CreateNamedPipe", Nothing);
    }

    // ACE's must be added to pipe's DACL for:
    // - remote clients are rejected.
    // - client processes running under low-privilege accounts are also
    //   able to change state of client end of this pipe to Non-Blocking
    //    and Message-Mode.

    PACL pACL = NULL;
    PACL pNewACL = NULL;
    EXPLICIT_ACCESS explicit_access_list[2];
    TRUSTEE trustee[2];
    DWORD e;

    // reject remote clients:
    trustee[0].TrusteeForm = TRUSTEE_IS_SID;
    trustee[0].TrusteeType = TRUSTEE_IS_GROUP;
    trustee[0].ptstrName = (LPTSTR) network_sid;
    trustee[0].MultipleTrusteeOperation = NO_MULTIPLE_TRUSTEE;
    trustee[0].pMultipleTrustee = NULL;

    ZeroMemory(&explicit_access_list[0], sizeof(EXPLICIT_ACCESS));
    explicit_access_list[0].grfAccessMode = SET_ACCESS;
    explicit_access_list[0].grfAccessPermissions = 0;
    explicit_access_list[0].grfInheritance = NO_INHERITANCE;
    explicit_access_list[0].Trustee = trustee[0];

    // allow all local users to connect:
    trustee[1].TrusteeForm = TRUSTEE_IS_SID;
    trustee[1].TrusteeType = TRUSTEE_IS_GROUP;
    trustee[1].ptstrName = (LPTSTR) world_sid;;
    trustee[1].MultipleTrusteeOperation = NO_MULTIPLE_TRUSTEE;
    trustee[1].pMultipleTrustee = NULL;

    ZeroMemory(&explicit_access_list[1], sizeof(EXPLICIT_ACCESS));
    explicit_access_list[1].grfAccessMode = GRANT_ACCESS;
    explicit_access_list[1].grfAccessPermissions = FILE_WRITE_ATTRIBUTES;
    explicit_access_list[1].grfInheritance = NO_INHERITANCE;
    explicit_access_list[1].Trustee = trustee[1];

    e = GetSecurityInfo(h, SE_KERNEL_OBJECT, DACL_SECURITY_INFORMATION, 
			NULL, NULL,
			&pACL, NULL, NULL);
    if (e != ERROR_SUCCESS) {
	win32_maperr(GetLastError());
	CloseHandle(h);
	uerror("netsys_create_local_named_pipe/GetSecurityInfo", Nothing);
    };

    e = SetEntriesInAcl(2, explicit_access_list, pACL, &pNewACL);
    if (e != ERROR_SUCCESS) {
	win32_maperr(GetLastError());
	CloseHandle(h);
	uerror("netsys_create_local_named_pipe/SetEntriesinAcl", Nothing);
    };

    e = SetSecurityInfo(h, SE_KERNEL_OBJECT, DACL_SECURITY_INFORMATION, 
			NULL, NULL, pNewACL, NULL);
    if (e != ERROR_SUCCESS) {
	win32_maperr(GetLastError());
	LocalFree(pNewACL);
	CloseHandle(h);
	uerror("netsys_create_local_named_pipe/SetSecurityInfo", Nothing);
    };

    LocalFree(pNewACL);

    /* - done with setting up security - */

    ph = alloc_pipe_helper(h, cn_ev);
    ph->pipe_is_server = 1;
    ph->pipe_mode_rd = mode_rd;
    ph->pipe_mode_wr = mode_wr;

    r = caml_alloc_custom(&pipe_helper_ops, sizeof(struct pipehelper *), 1, 0);
    *(pipe_helper_ptr_val(r)) = ph;

    dprintf("PIPE create_local_named_pipe %u successful\n",
	    ph->pipe_handle);
    return r;
#else
    invalid_argument("netsys_create_local_named_pipe");
#endif
}


CAMLprim value netsys_pipe_listen(value phv) {
#ifdef _WIN32
    struct pipe_helper *ph;
    int flag, set_cn_ev;
    DWORD err;

    ph = *(pipe_helper_ptr_val(phv));
    
    dprintf("PIPE listen %u\n",
	    ph->pipe_handle);

    if (ph->pipe_is_open)
	check_for_pending_operations(ph);

    if (!ph->pipe_is_open) {
	errno = EBADF;
	uerror("netsys_pipe_listen", Nothing);
    };

    if (!ph->pipe_is_server) {
	errno = EPERM;
	uerror("netsys_pipe_listen", Nothing);
    };

    if (ph->pipe_conn_state != PIPE_DEAF) {
	errno = EISCONN;
	uerror("netsys_pipe_listen", Nothing);
    };

    if (ph->pipe_cn_ovrlp_started) {
	errno = EALREADY;
	uerror("netsys_pipe_listen", Nothing);
    };

    dprintf("PIPE listen %u connecting\n",
	    ph->pipe_handle);

    /* ConnectNamedPipe resets pipe_cn_ev. Because pipe_cn_ev may be
       shared by several pipes, this behavior is in the way. We have to
       fix that by setting pipe_cn_ev again after ConnectNamedPipe if it
       was set before.
    */
    set_cn_ev = 0;
    if (ph->pipe_cn_ev != INVALID_HANDLE_VALUE) {
	DWORD n;
	n = WaitForSingleObject(ph->pipe_cn_ev, 0);
	if (n == WAIT_FAILED) {
	    win32_maperr(GetLastError());
	    uerror("netsys_pipe_listen/WaitForSingleObject", Nothing);
	};
	if (n == WAIT_OBJECT_0)
	    set_cn_ev = 1;
    };

    flag = ConnectNamedPipe(ph->pipe_handle,
			    ph->pipe_cn_ovrlp);
    if (flag) {
	/* immediate success */
	ph->pipe_conn_state = PIPE_CONNECTED;
	set_cn_ev = 1;
	dprintf("PIPE listen %u connected 1\n",
		ph->pipe_handle);
	
    } else {
	err = GetLastError();
	switch (err) {
	case ERROR_PIPE_CONNECTED:
	    /* also immediate success */
	    ph->pipe_conn_state = PIPE_CONNECTED; 
	    set_cn_ev = 1;
	    dprintf("PIPE listen %u connected 2\n",
		    ph->pipe_handle);
	    break;
	case ERROR_IO_PENDING:
	    /* we connect in the background */
	    ph->pipe_cn_ovrlp_started = 1; 
	    ph->pipe_conn_state = PIPE_LISTENING;
	    dprintf("PIPE listen %u pending\n",
		    ph->pipe_handle);
	    break;
	default:
	    dprintf("PIPE listen %u error err=%u\n",
		    ph->pipe_handle, err);
	    win32_maperr(err);
	    uerror("netsys_pipe_listen/ConnectNamedPipe", Nothing);
	};
    };

    if (set_cn_ev && ph->pipe_cn_ev != INVALID_HANDLE_VALUE)
	SetEvent(ph->pipe_cn_ev);

    return Val_unit;
#else
    invalid_argument("netsys_pipe_listen");
#endif
}


CAMLprim value netsys_pipe_deafen(value phv) {
#ifdef _WIN32
    struct pipe_helper *ph;
    int flag;

    ph = *(pipe_helper_ptr_val(phv));
    
    dprintf("PIPE deafen %u\n",
	    ph->pipe_handle);

    if (ph->pipe_is_open)
	check_for_pending_operations(ph);

    if (!ph->pipe_is_open) {
	errno = EBADF;
	uerror("netsys_pipe_deafen", Nothing);
    };

    if (!ph->pipe_is_server) {
	errno = EPERM;
	uerror("netsys_pipe_deafen", Nothing);
    };

    if (ph->pipe_conn_state == PIPE_DEAF) {
	errno = ENOTCONN;
	uerror("netsys_pipe_deafen", Nothing);
    };

    dprintf("PIPE deafen %u disconnecting\n",
	    ph->pipe_handle);

    flag = DisconnectNamedPipe(ph->pipe_handle);
    if (!flag) {
	win32_maperr(GetLastError());
	uerror("netsys_pipe_deafen/DisconnectNamedPipe", Nothing);
    }

    /* Check whether the overlapped ops are done: */
    check_for_pending_operations(ph);
    if (ph->pipe_cn_ovrlp_started) {
	failwith("netsys_pipe_unlisten: cannot stop pending ConnectNamedPipe");
    };
    if (ph->pipe_rd_ovrlp_started) {
	failwith("netsys_pipe_unlisten: cannot stop pending ReadFile");
    };
    if (ph->pipe_wr_ovrlp_started) {
	failwith("netsys_pipe_unlisten: cannot stop pending WriteFile");
    };

    dprintf("PIPE deafen %u successful\n",
	    ph->pipe_handle);

    ph->pipe_conn_state = PIPE_DEAF;
    ph->pipe_error_rd = 0;
    ph->pipe_error_wr = 0;
    ph->pipe_rd_buf_size = 0;
    ph->pipe_rd_eof = 0;
    ph->pipe_wr_buf_size = 0;

    ResetEvent(ph->pipe_rd_ev);
    ResetEvent(ph->pipe_wr_ev);
    if (ph->pipe_cn_ev != INVALID_HANDLE_VALUE)
	ResetEvent(ph->pipe_cn_ev);

    return Val_unit;

#else
    invalid_argument("netsys_pipe_unlisten");
#endif
}


CAMLprim value netsys_pipe_connect(value name, value mode) {
#ifdef _WIN32
    HANDLE h;
    DWORD omode;
    int mode_rd, mode_wr;
    DWORD err;
    struct pipe_helper *ph;
    value r;

    omode = 0;
    mode_rd = 0;
    mode_wr = 0;
    switch (Int_val(mode)) {
    case 0: /* Pipe_in */
	omode = GENERIC_READ; mode_rd = 1; break;
    case 1: /* Pipe_out */
	omode = GENERIC_WRITE; mode_wr = 1; break;
    case 2: /* Pipe_duplex */
	omode = GENERIC_READ | GENERIC_WRITE; mode_rd = 1; mode_wr = 1; break;
    };

    /* SECURITY_SQOS_PRESENT | SECURITY_ANONYMOUS: This prevents that the
       pipe server can impersonate as the calling user. If this is not
       excluded, the server has access to the caller's security context, and
       can run code in this context.

       For remote named pipes, these flags are ignored! For that reason,
       we have to restrict this function to local named pipes only (this is
       done in Ocaml).
    */
    h = CreateFile(String_val(name),
		   omode,
		   0,
		   NULL,
		   OPEN_EXISTING,
		   FILE_FLAG_OVERLAPPED | 
		     SECURITY_SQOS_PRESENT | SECURITY_ANONYMOUS,
		   NULL);
    if ( h == INVALID_HANDLE_VALUE ) {
	err = GetLastError();
	if ( err == ERROR_PIPE_BUSY )
	    errno = EAGAIN;
	else
	    win32_maperr(err);
	uerror("netsys_pipe_connect/CreateFile", Nothing);
    };

    ph = alloc_pipe_helper(h, INVALID_HANDLE_VALUE);
    ph->pipe_conn_state = PIPE_CONNECTED;
    ph->pipe_mode_rd = mode_rd;
    ph->pipe_mode_wr = mode_wr;
    SetEvent(ph->pipe_wr_ev);

    r = caml_alloc_custom(&pipe_helper_ops, sizeof(struct pipehelper *), 1, 0);
    *(pipe_helper_ptr_val(r)) = ph;

    dprintf("PIPE connect %u successful\n",
	    ph->pipe_handle);

    if (mode_rd)
	start_reading(ph);

    return r;

#else
    invalid_argument("netsys_pipe_connect");
#endif
}


CAMLprim value netsys_pipe_read(value phv, value s, value pos, value len) {
#ifdef _WIN32
    struct pipe_helper *ph;
    int flag;
    int l;

    ph = *(pipe_helper_ptr_val(phv));
    l = Int_val(len);
    
    dprintf("PIPE read %u len=%d\n",
	    ph->pipe_handle, l);

    if (ph->pipe_is_open)
	check_for_pending_operations(ph);

    if (ph->pipe_error_rd != 0) {
	win32_maperr(ph->pipe_error_rd);
	uerror("netsys_pipe_read", Nothing);
    };

    if (l == 0) 
	return Val_int(0);

    if (!ph->pipe_is_open || !ph->pipe_mode_rd) {
	errno = EBADF;
	uerror("netsys_pipe_read", Nothing);
    };

    if (ph->pipe_conn_state != PIPE_CONNECTED) {
	errno = ENOTCONN;
	uerror("netsys_pipe_read", Nothing);
    };

    if (ph->pipe_rd_ovrlp_started) {
	dprintf("PIPE read %u eagain\n",
		ph->pipe_handle);

	errno = EAGAIN;
	uerror("netsys_pipe_read", Nothing);
    };

    if (ph->pipe_rd_buf_size < l) 
	l = ph->pipe_rd_buf_size;

    CopyMemory(String_val(s) + Int_val(pos), ph->pipe_rd_buf, l);
    MoveMemory(ph->pipe_rd_buf, ph->pipe_rd_buf+l, ph->pipe_rd_buf_size-l);
    ph->pipe_rd_buf_size = ph->pipe_rd_buf_size - l;

    if (ph->pipe_rd_buf_size == 0 && !ph->pipe_rd_eof)
	start_reading(ph);

    dprintf("PIPE read %u returning %d\n",
	    ph->pipe_handle, l);

    return Val_int(l);
#else
    invalid_argument("netsys_pipe_read");
#endif
}


CAMLprim value netsys_pipe_write(value phv, value s, value pos, value len) {
#ifdef _WIN32
    struct pipe_helper *ph;
    int flag;
    int l;

    ph = *(pipe_helper_ptr_val(phv));
    l = Int_val(len);
    
    dprintf("PIPE write %u %d\n",
	    ph->pipe_handle, l);

    if (ph->pipe_is_open)
	check_for_pending_operations(ph);

    if (ph->pipe_error_wr != 0) {
	if (ph->pipe_error_wr == ERROR_PIPE_NOT_CONNECTED ||
	    ph->pipe_error_wr == ERROR_NO_DATA
	    ) {
	    errno = EPIPE;
	    dprintf("PIPE write %u epipe\n",
		    ph->pipe_handle);
	}
	else
	    win32_maperr(ph->pipe_error_wr);
	uerror("netsys_pipe_write", Nothing);
    };

    if (l == 0) 
	return Val_int(0);

    if (!ph->pipe_is_open || !ph->pipe_mode_wr) {
	errno = EBADF;
	uerror("netsys_pipe_write", Nothing);
    };

    if (ph->pipe_conn_state != PIPE_CONNECTED) {
	errno = ENOTCONN;
	uerror("netsys_pipe_write", Nothing);
    };

    if (ph->pipe_wr_ovrlp_started) {
	dprintf("PIPE write %u eagain\n",
	       ph->pipe_handle);
	errno = EAGAIN;
	uerror("netsys_pipe_write", Nothing);
    };

    if (l > PIPE_HELPER_BUF_SIZE) 
	l = PIPE_HELPER_BUF_SIZE;

    CopyMemory(ph->pipe_wr_buf, String_val(s) + Int_val(pos), l);
    ph->pipe_wr_buf_size = l;

    if (l > 0)
	start_writing(ph);

    dprintf("PIPE write %u returning %d\n",
	    ph->pipe_handle, l);

    return Val_int(l);
#else
    invalid_argument("netsys_pipe_write");
#endif
}


CAMLprim value netsys_pipe_shutdown(value phv) {
#ifdef _WIN32
    struct pipe_helper *ph;
    int flag;

    ph = *(pipe_helper_ptr_val(phv));

    dprintf("PIPE shutdown %u is_open=%d\n",
	    ph->pipe_handle, ph->pipe_is_open);

    if (ph->pipe_is_open) {
	if (ph->pipe_conn_state == PIPE_DEAF) {
	    errno = ENOTCONN;
	    uerror("netsys_pipe_shutdown", Nothing);
	};

	flag = CloseHandle(ph->pipe_handle);
	if (!flag) {
	    win32_maperr(GetLastError());
	    uerror("netsys_pipe_shutdown/CloseHandle", Nothing);
	};
	ph->pipe_is_open = 0;
	ph->pipe_conn_state = PIPE_DOWN;
	if (ph->pipe_signal != NULL)
	    SetEvent(ph->pipe_signal);
	ResetEvent(ph->pipe_rd_ev);
	ResetEvent(ph->pipe_wr_ev);
	if (ph->pipe_cn_ev != INVALID_HANDLE_VALUE)
	    ResetEvent(ph->pipe_cn_ev);
    }

    return Val_unit;
#else
    invalid_argument("netsys_pipe_shutdown");
#endif
}


CAMLprim value netsys_pipe_free(value phv) {
#ifdef _WIN32
    struct pipe_helper *ph;

    ph = *(pipe_helper_ptr_val(phv));
    free_pipe_helper(ph);
    *(pipe_helper_ptr_val(phv)) = NULL;

    return Val_unit;
#else
    invalid_argument("netsys_pipe_free");
#endif
}


CAMLprim value netsys_pipe_conn_state(value phv) {
#ifdef _WIN32
    struct pipe_helper *ph;

    ph = *(pipe_helper_ptr_val(phv));

    if (ph->pipe_is_open)
	check_for_pending_operations(ph);

    return Val_int(ph->pipe_conn_state);
#else
    invalid_argument("netsys_pipe_conn_state");
#endif
}


CAMLprim value netsys_pipe_rd_event(value phv) {
#ifdef _WIN32
    struct pipe_helper *ph;

    ph = *(pipe_helper_ptr_val(phv));

    if (!ph->pipe_is_open) {
	errno = EBADF;
	uerror("netsys_pipe_rd_event", Nothing);
    };

    return alloc_event(ph->pipe_rd_ev);
#else
    invalid_argument("netsys_pipe_rd_event");
#endif
}


CAMLprim value netsys_pipe_wr_event(value phv) {
#ifdef _WIN32
    struct pipe_helper *ph;

    ph = *(pipe_helper_ptr_val(phv));

    if (!ph->pipe_is_open) {
	errno = EBADF;
	uerror("netsys_pipe_wr_event", Nothing);
    };

    return alloc_event(ph->pipe_wr_ev);
#else
    invalid_argument("netsys_pipe_wr_event");
#endif
}


CAMLprim value netsys_pipe_descr(value phv) {
#ifdef _WIN32
    struct pipe_helper *ph;

    ph = *(pipe_helper_ptr_val(phv));

    return netsysw32_win_alloc_handle(ph->pipe_descr);
#else
    invalid_argument("netsys_pipe_descr");
#endif
}

CAMLprim value netsys_set_auto_close_pipe_proxy(value phv, value flag) {
#ifdef _WIN32
    struct pipe_helper *ph;

    ph = *(pipe_helper_ptr_val(phv));
    ph->pipe_descr_auto_close = Bool_val(flag);
    return Val_unit;
#else
    invalid_argument("netsys_set_auto_close_pipe_proxy");
#endif
}


CAMLprim value netsys_pipe_signal(value phv, value ev) {
#ifdef _WIN32
    struct pipe_helper *ph;
    struct event e;

    ph = *(pipe_helper_ptr_val(phv));
    e = *(event_val(ev));
    ph->pipe_signal = e.ev;

    return Val_unit;
#else
    invalid_argument("netsys_pipe_signal");
#endif
}


#ifdef _WIN32
struct process {
    HANDLE proc;        /* INVALID_HANDLE_VALUE if already closed */
    HANDLE proc_proxy;  /* the proxy descriptor */
    int    auto_close;
    DWORD  win_pid;
};

#define process_val(v) ((struct process *) (Data_custom_val(v)))

static struct custom_operations process_ops = {
    "",
    custom_finalize_default,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default
};

static value alloc_process(HANDLE proc, DWORD win_pid) {
    value r;
    struct process *p0;
    int flag;
    HANDLE e_proxy;

    r = caml_alloc_custom(&process_ops, sizeof(struct process), 1, 0);
    p0 = process_val(r);

    e_proxy = CreateEvent(NULL, 1, 0, NULL);
    if (e_proxy == NULL) {
	win32_maperr(GetLastError());
	uerror("alloc_process/CreateEvent", Nothing);
    };

    p0->proc = proc;
    p0->proc_proxy = e_proxy;
    p0->auto_close = 1;
    p0->win_pid = win_pid;

    return r;
}


static int has_console(void) {
    HANDLE h;

    h = CreateFile("CONOUT$", GENERIC_WRITE, FILE_SHARE_WRITE, NULL,
		   OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
    if (h == INVALID_HANDLE_VALUE) {
	return 0;
    } else {
	CloseHandle(h);
	return 1;
    }
}
#endif /* _WIN32 */


CAMLprim value netsys_create_process(value cmd,
				     value cmdline,
				     value opts) {
#ifdef _WIN32
  value opts_hd;
  PROCESS_INFORMATION pi;
  STARTUPINFO si;
  char *chdir;
  char *envp;
  int flags;
  int pass_std_handles;
  int console_flags;
  int env_flags;
  int pg_flags;
  int code;
  value v;

  chdir = NULL;
  envp = NULL;
  GetStartupInfo(&si);
  si.dwFlags &= ~STARTF_USESTDHANDLES;
  flags = 0;
  pass_std_handles = 0;
  console_flags = 0;
  env_flags = 0;
  pg_flags = 0;
  
  dprintf("netsys_create_process cmd=%s cmdline=%s\n",
	  String_val(cmd), String_val(cmdline));

  /* Iterate over opts: */
  opts_hd = opts;
  while (opts_hd != Val_int(0)) {
      v = Field(opts_hd,0);
      if (Is_block(v)) {
	  switch(Tag_val(v)) {
	  case 0: /* CP_change_directory */
	      chdir = String_val(Field(v,0));
	      break;
	  case 1: /* CP_set_env */
	      envp = String_val(Field(v,0));
	      break;
	  case 2: /* CP_std_handles */
	      si.hStdInput = Handle_val(Field(v,0));
	      si.hStdOutput = Handle_val(Field(v,1));
	      si.hStdError = Handle_val(Field(v,2));
	      pass_std_handles = 1;
	      break;
	  default:
	      invalid_argument("netsys_create_process [1]");
	  }
      }
      else {
	  switch (Int_val(v)) {
	  case 0: /* CP_create_console */
	      console_flags = CREATE_NEW_CONSOLE;
	      break;
	  case 1: /* CP_detach_from_console */
	      console_flags = DETACHED_PROCESS;
	      break;
	  case 2: /* CP_inherit_console */
	      console_flags = 0;
	      break;
	  case 3: /* CP_inherit_or_create_console */
	      console_flags = has_console() ? 0 : CREATE_NEW_CONSOLE;
	  case 4: /* CP_unicode_environment */
	      env_flags = CREATE_UNICODE_ENVIRONMENT;
	      break;
	  case 5: /* CP_ansi_environment */
	      env_flags = 0;
	      break;
	  case 6: /* CP_new_process_group */
	      pg_flags = CREATE_NEW_PROCESS_GROUP;
	      break;
	  case 7: /* CP_inherit_process_group */
	      pg_flags = 0;
	      break;
	  default:
	      invalid_argument("netsys_create_process [2]");
	  }
      };
      opts_hd = Field(opts_hd,1);
  }

  if (pass_std_handles) 
      si.dwFlags |= STARTF_USESTDHANDLES;
  flags |= console_flags | env_flags | pg_flags;

  code = CreateProcess(String_val(cmd),
		       String_val(cmdline),
		       NULL,
		       NULL,
		       TRUE,
		       flags,
		       envp,
		       chdir,
		       &si,
		       &pi);
  if (!code) {
      win32_maperr(GetLastError());
      uerror("create_process/CreateProcess", cmd);
  };
  CloseHandle(pi.hThread);
  dprintf("netsys_create_process hProcess=%u processId=%u\n",
	  pi.hProcess, pi.dwProcessId);
  return alloc_process(pi.hProcess, pi.dwProcessId);
#else
    invalid_argument("netsys_create_process");
#endif
}


CAMLprim value netsys_search_path(value path_opt_v, 
				  value file_v,
				  value ext_opt_v) {
#ifdef _WIN32
    char *path;
    char *file;
    char *ext;
    char *fullname;
    DWORD pathlen, code;
    int cont;
    value r;

    if (Is_block(path_opt_v))
	path = String_val(Field(path_opt_v,0));
    else
	path = NULL;

    file = String_val(file_v);

    if (Is_block(ext_opt_v))
	ext = String_val(Field(ext_opt_v,0));
    else
	ext = NULL;

    caml_enter_blocking_section();    /* searching can take some time */
    
    pathlen = strlen(file) + 1;
    if (pathlen < 256) pathlen = 256;
    cont = 1;
    while (cont) {
	fullname = stat_alloc(pathlen);
	code = SearchPath(path,
			  file,
			  ext,
			  pathlen,
			  fullname,
			  NULL);
	cont = (code >= pathlen);
	if (cont) {
	    stat_free(fullname);
	    pathlen = code+1;  /* space for NULL byte! */
	}
    }
    
    caml_leave_blocking_section();

    if (code == 0) {
	stat_free(fullname);
	errno = ENOENT;
	uerror("netsys_search_path", file_v);
    };

    r = caml_copy_string(fullname);
    stat_free(fullname);
    return r;
#else
    invalid_argument("netsys_search_path");
#endif
}


CAMLprim value netsys_terminate_process(value pv) {
#ifdef _WIN32
    struct process *p0;
    p0 = process_val(pv);
    dprintf("netsys_terminate_process hProcess=%u processId=%u\n",
	    p0->proc, p0->win_pid);
    if (!TerminateProcess(p0->proc, 126)) {
      win32_maperr(GetLastError());
      uerror("terminate_process/TerminateProcess", Nothing);
    }
    return Val_unit;
#else
    invalid_argument("netsys_terminate_process");
#endif
}




CAMLprim value netsys_process_descr(value pv) {
#ifdef _WIN32
    struct process *p0;
    p0 = process_val(pv);
    return netsysw32_win_alloc_handle(p0->proc_proxy);
#else
    invalid_argument("netsys_process_descr");
#endif
}


CAMLprim value netsys_set_auto_close_process_proxy(value pv, value flag) {
#ifdef _WIN32
    struct process *p0;
    p0 = process_val(pv);
    p0->auto_close = Bool_val(flag);
    return Val_unit;
#else
    invalid_argument("netsys_set_auto_close_process_proxy");
#endif
}


CAMLprim value netsys_close_process(value pv) {
#ifdef _WIN32
    struct process *p0;
    p0 = process_val(pv);
    if (p0->proc != INVALID_HANDLE_VALUE) {
	CloseHandle(p0->proc);
	p0->proc = INVALID_HANDLE_VALUE;
    };
    return Val_unit;
#else
    invalid_argument("netsys_close_process");
#endif
}

CAMLprim value netsys_process_free(value pv) {
#ifdef _WIN32
    struct process *p0;
    p0 = process_val(pv);
    if (p0->proc != INVALID_HANDLE_VALUE)
	CloseHandle(p0->proc);
    if (p0->auto_close)
	CloseHandle(p0->proc_proxy);
    stat_free(p0);
    return Val_unit;
#else
    invalid_argument("netsys_close_process");
#endif
}


CAMLprim value netsys_get_process_status(value pv) {
#ifdef _WIN32
    struct process *p0;
    DWORD code, status;

    p0 = process_val(pv);
    /* First test whether the process is still running: */
    code = WaitForSingleObject(p0->proc, 0);
    if (code == WAIT_TIMEOUT) {
	caml_raise_not_found();
    };
    if (code == WAIT_FAILED) {
	win32_maperr(GetLastError());
	uerror("netsys_get_process_status/WaitForSingleObject", Nothing);
    };
    if (code != WAIT_OBJECT_0) {
	invalid_argument("netsys_get_process_status [1]");
    };
    /* Now get the status: */
    code = GetExitCodeProcess(p0->proc, &status);
    if (!code) {
	win32_maperr(GetLastError());
	uerror("netsys_get_process_status/getExitCodeProcess", Nothing);
    }
    
    return Val_int(status);

#else
    invalid_argument("netsys_get_process_status");
#endif
}


CAMLprim value netsys_as_process_event(value pv) {
#ifdef _WIN32
    struct process *p0;
    p0 = process_val(pv);
    return alloc_event(p0->proc);
#else
    invalid_argument("netsys_as_process_event");
#endif
}


CAMLprim value netsys_emulated_pid(value pv) {
#ifdef _WIN32
    struct process *p0;
    HANDLE d;
    p0 = process_val(pv);
    if(!DuplicateHandle(GetCurrentProcess(),
			p0->proc, 
			GetCurrentProcess(),
			&d, 
			0,
			FALSE,
			DUPLICATE_SAME_ACCESS)) {
	win32_maperr(GetLastError());
	uerror("netsys_emulated_pid/DuplicateHandle", Nothing);
    };
    return Val_int(d);
#else
    invalid_argument("netsys_emulated_pid");
#endif
}


CAMLprim value netsys_win_pid(value pv) {
#ifdef _WIN32
    struct process *p0;
    p0 = process_val(pv);
    return Val_int(p0->win_pid);
#else
    invalid_argument("netsys_win_pid");
#endif
}


CAMLprim value netsys_has_console(value dummy) {
#ifdef _WIN32
    return Val_bool(has_console());
#else
    invalid_argument("netsys_has_console");
#endif
}


CAMLprim value netsys_is_console(value fd) {
#ifdef _WIN32
    DWORD mode;

    if (!GetConsoleMode(Handle_val(fd), &mode))
	/* CHECK: which error is typical here? */
	return Val_bool(0);
    else
	return Val_bool(1);
#else
    invalid_argument("netsys_is_console");
#endif
}

CAMLprim value netsys_alloc_console(value dummy) {
#ifdef _WIN32
    if (!AllocConsole()) {
	win32_maperr(GetLastError());
	uerror("netsys_alloc_console/AllocConsole", Nothing);
    }
    return Val_unit;
#else
    invalid_argument("netsys_alloc_console");
#endif
}

CAMLprim value netsys_get_console_attr(value dummy) {
#ifdef _WIN32
    HANDLE conout;
    CONSOLE_CURSOR_INFO cci;
    CONSOLE_SCREEN_BUFFER_INFO csbi;
    value r;

    conout = CreateFile("CONOUT$", GENERIC_READ | GENERIC_WRITE, 
			FILE_SHARE_WRITE, NULL,
			OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
    if (conout == INVALID_HANDLE_VALUE) {
	win32_maperr(GetLastError());
	uerror("netsys_get_console_attr/CreateFile", Nothing);
    }
    if (!GetConsoleCursorInfo(conout, &cci)) {
	win32_maperr(GetLastError());
	CloseHandle(conout);
	uerror("netsys_get_console_attr/GetConsoleCursorInfo", Nothing);
    }
    if (!GetConsoleScreenBufferInfo(conout, &csbi)) {
	win32_maperr(GetLastError());
	CloseHandle(conout);
	uerror("netsys_get_console_attr/GetConsoleScreenBufferInfo", Nothing);
    }
    CloseHandle(conout);

    r = caml_alloc_tuple(5);
    Field(r,0) = Val_int(csbi.dwCursorPosition.X - csbi.srWindow.Left);
    Field(r,1) = Val_int(csbi.dwCursorPosition.Y - csbi.srWindow.Top );
    Field(r,2) = Val_int(cci.dwSize);
    Field(r,3) = Val_bool(cci.bVisible);
    Field(r,4) = Val_int(csbi.wAttributes);

    return r;
#else
    invalid_argument("netsys_get_console_attr");
#endif
}


CAMLprim value netsys_set_console_attr(value av) {
#ifdef _WIN32
    HANDLE conout;
    COORD pos;
    CONSOLE_CURSOR_INFO cci;
    CONSOLE_SCREEN_BUFFER_INFO csbi;
    int cx, cy, csize, cvisible, tattr;

    cx = Int_val(Field(av, 0));
    cy = Int_val(Field(av, 1));
    csize = Int_val(Field(av, 2));
    cvisible = Bool_val(Field(av, 3));
    tattr = Int_val(Field(av, 4));

    conout = CreateFile("CONOUT$", GENERIC_READ | GENERIC_WRITE, 
			FILE_SHARE_WRITE, NULL,
			OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
    if (conout == INVALID_HANDLE_VALUE) {
	win32_maperr(GetLastError());
	uerror("netsys_set_console_attr/CreateFile", Nothing);
    }
    
    cci.dwSize = csize;
    cci.bVisible = cvisible;
    if (!SetConsoleCursorInfo(conout, &cci)) {
	win32_maperr(GetLastError());
	CloseHandle(conout);
	uerror("netsys_set_console_attr/SetConsoleCursorInfo", Nothing);
    }
  
    if (!GetConsoleScreenBufferInfo(conout, &csbi)) {
	win32_maperr(GetLastError());
	CloseHandle(conout);
	uerror("netsys_set_console_attr/GetConsoleScreenBufferInfo", Nothing);
    }

    pos.X = cx + csbi.srWindow.Left;
    pos.Y = cy + csbi.srWindow.Top;
    if (!SetConsoleCursorPosition(conout, pos)) {
	win32_maperr(GetLastError());
	CloseHandle(conout);
	uerror("netsys_set_console_attr/SetConsoleCursorPosition", Nothing);
    }

    if (!SetConsoleTextAttribute(conout, tattr)) {
	win32_maperr(GetLastError());
	CloseHandle(conout);
	uerror("netsys_set_console_attr/SetConsoleTextAttributes", Nothing);
    }

    CloseHandle(conout);
  
    return Val_unit;
#else
    invalid_argument("netsys_set_console_attr");
#endif
}


CAMLprim value netsys_get_console_info(value dummy) {
#ifdef _WIN32
    HANDLE conout;
    CONSOLE_SCREEN_BUFFER_INFO csbi;
    value r;

    conout = CreateFile("CONOUT$", GENERIC_READ | GENERIC_WRITE, 
			FILE_SHARE_WRITE, NULL,
			OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
    if (conout == INVALID_HANDLE_VALUE) {
	win32_maperr(GetLastError());
	uerror("netsys_get_console_info/CreateFile", Nothing);
    }
    
    if (!GetConsoleScreenBufferInfo(conout, &csbi)) {
	win32_maperr(GetLastError());
	CloseHandle(conout);
	uerror("netsys_get_console_info/GetConsoleScreenBufferInfo", Nothing);
    }
    CloseHandle(conout);

    r = caml_alloc_tuple(2);
    Field(r,0) = Val_int(csbi.srWindow.Right - csbi.srWindow.Left + 1);
    Field(r,1) = Val_int(csbi.srWindow.Bottom - csbi.srWindow.Top + 1);

    return r;
#else
    invalid_argument("netsys_get_console_info");
#endif
}


CAMLprim value netsys_get_console_mode(value dummy) {
#ifdef _WIN32
    HANDLE conin, conout;
    DWORD modein, modeout;
    value r;

    conin = CreateFile("CONIN$", GENERIC_READ | GENERIC_WRITE, 
		       FILE_SHARE_READ, NULL,
		       OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
    if (conin == INVALID_HANDLE_VALUE) {
	win32_maperr(GetLastError());
	uerror("netsys_get_console_mode/CreateFile", Nothing);
    }
    
    if (!GetConsoleMode(conin, &modein)) {
	win32_maperr(GetLastError());
	CloseHandle(conin);
	uerror("netsys_get_console_mode/GetConsoleMode", Nothing);
    }

    CloseHandle(conin);

    conout = CreateFile("CONOUT$", GENERIC_READ | GENERIC_WRITE, 
			FILE_SHARE_WRITE, NULL,
			OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
    if (conout == INVALID_HANDLE_VALUE) {
	win32_maperr(GetLastError());
	uerror("netsys_get_console_mode/CreateFile", Nothing);
    }
    
    if (!GetConsoleMode(conout, &modeout)) {
	win32_maperr(GetLastError());
	CloseHandle(conout);
	uerror("netsys_get_console_mode/GetConsoleMode", Nothing);
    }

    CloseHandle(conout),
    
    r = caml_alloc_tuple(7);
    Field(r,0) = Val_bool(modein & ENABLE_ECHO_INPUT);
    Field(r,1) = Val_bool(modein & ENABLE_INSERT_MODE);
    Field(r,2) = Val_bool(modein & ENABLE_LINE_INPUT);
    Field(r,3) = Val_bool(modein & ENABLE_PROCESSED_INPUT);
    Field(r,4) = Val_bool(modein & ENABLE_QUICK_EDIT_MODE);
    Field(r,5) = Val_bool(modeout & ENABLE_PROCESSED_OUTPUT);
    Field(r,6) = Val_bool(modeout & ENABLE_WRAP_AT_EOL_OUTPUT);

    return r;
#else
    invalid_argument("netsys_get_console_mode");
#endif
}


CAMLprim value netsys_set_console_mode(value mv) {
#ifdef _WIN32
    HANDLE conin, conout;
    DWORD modein, modeout;
    value r;

    conin = CreateFile("CONIN$", GENERIC_READ | GENERIC_WRITE, 
		       FILE_SHARE_READ, NULL,
		       OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
    if (conin == INVALID_HANDLE_VALUE) {
	win32_maperr(GetLastError());
	uerror("netsys_set_console_mode/CreateFile", Nothing);
    }
    
    if (!GetConsoleMode(conin, &modein)) {
	win32_maperr(GetLastError());
	CloseHandle(conin);
	uerror("netsys_set_console_mode/GetConsoleMode", Nothing);
    }

    modein &= ~ENABLE_ECHO_INPUT & ~ENABLE_INSERT_MODE & ~ENABLE_LINE_INPUT &
	~ENABLE_PROCESSED_INPUT & ~ENABLE_QUICK_EDIT_MODE;
    
    if (Bool_val(Field(mv, 0))) modein |= ENABLE_ECHO_INPUT;
    if (Bool_val(Field(mv, 1))) modein |= ENABLE_INSERT_MODE;
    if (Bool_val(Field(mv, 2))) modein |= ENABLE_LINE_INPUT;
    if (Bool_val(Field(mv, 3))) modein |= ENABLE_PROCESSED_INPUT;
    if (Bool_val(Field(mv, 4))) modein |= ENABLE_QUICK_EDIT_MODE;
    
    if (!SetConsoleMode(conin, modein)) {
	win32_maperr(GetLastError());
	CloseHandle(conin);
	uerror("netsys_set_console_mode/SetConsoleMode", Nothing);
    };

    CloseHandle(conin);

    conout = CreateFile("CONOUT$", GENERIC_READ | GENERIC_WRITE, 
			FILE_SHARE_WRITE, NULL,
			OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
    if (conout == INVALID_HANDLE_VALUE) {
	win32_maperr(GetLastError());
	uerror("netsys_set_console_mode/CreateFile", Nothing);
    }
    
    if (!GetConsoleMode(conout, &modeout)) {
	win32_maperr(GetLastError());
	CloseHandle(conout);
	uerror("netsys_set_console_mode/GetConsoleMode", Nothing);
    }

    modeout &= ~ENABLE_PROCESSED_OUTPUT & ~ENABLE_WRAP_AT_EOL_OUTPUT;
    if (Bool_val(Field(mv, 5))) modeout |= ENABLE_PROCESSED_OUTPUT;
    if (Bool_val(Field(mv, 6))) modeout |= ENABLE_WRAP_AT_EOL_OUTPUT;
    
    if (!SetConsoleMode(conout, modeout)) {
	win32_maperr(GetLastError());
	CloseHandle(conout);
	uerror("netsys_set_console_mode/SetConsoleMode", Nothing);
    };

    CloseHandle(conout);
    return Val_unit;
#else
    invalid_argument("netsys_set_console_mode");
#endif
}


CAMLprim value netsys_getacp(value dummy) {
#ifdef _WIN32
    return Val_int(GetACP());
#else
    invalid_argument("netsys_getacp");
#endif
}


CAMLprim value netsys_init_console_codepage(value dummy) {
#ifdef _WIN32
    int cp;
    cp = GetACP();
    if (!SetConsoleCP(cp)) {
	win32_maperr(GetLastError());
	uerror("netsys_init_console_codepage/SetConsoleCP", Nothing);
    }
    if (!SetConsoleOutputCP(cp)) {
	win32_maperr(GetLastError());
	uerror("netsys_init_console_codepage/SetConsoleOutputCP", Nothing);
    }
    return Val_unit;
#else
    invalid_argument("netsys_init_console_codepage");
#endif
}


#ifdef _WIN32
static void clear_eol(HANDLE conout, COORD p, DWORD right, int attr) {
    DWORD n, nact;

    n = right - p.X + 1;
    if (!FillConsoleOutputCharacter(conout, 32, n, 
				    p, &nact)) {
	win32_maperr(GetLastError());
	CloseHandle(conout);
	uerror("netsys_clear_console/FillConsoleOutputCharacter", Nothing);
    }
    if (!FillConsoleOutputAttribute(conout, attr, n, 
				    p, &nact)) {
	win32_maperr(GetLastError());
	CloseHandle(conout);
	uerror("netsys_clear_console/FillConsoleOutputAttribute", Nothing);
    }
}
#endif


CAMLprim value netsys_clear_console(value mode) {
#ifdef _WIN32
    HANDLE conout;
    CONSOLE_SCREEN_BUFFER_INFO csbi;
    COORD p;
    int width, height;
    SMALL_RECT new_win;

    conout = CreateFile("CONOUT$", GENERIC_READ | GENERIC_WRITE, 
			FILE_SHARE_WRITE, NULL,
			OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
    if (conout == INVALID_HANDLE_VALUE) {
	win32_maperr(GetLastError());
	uerror("netsys_clear_console/CreateFile", Nothing);
    }

    if (!GetConsoleScreenBufferInfo(conout, &csbi)) {
	win32_maperr(GetLastError());
	CloseHandle(conout);
	uerror("netsys_clear_console/GetConsoleScreenBufferInfo", Nothing);
    }

    switch (Int_val(mode)) {
    case 0:  /* EOL */
	clear_eol(conout, csbi.dwCursorPosition, csbi.srWindow.Right,
		  csbi.wAttributes);
	break;
    case 1:  /* EOS */
	p = csbi.dwCursorPosition;
	clear_eol(conout, csbi.dwCursorPosition, csbi.srWindow.Right,
		  csbi.wAttributes);
	while (p.Y < csbi.srWindow.Bottom) {
	    p.Y ++;
	    p.X = csbi.srWindow.Left;
	    clear_eol(conout, p, csbi.srWindow.Right, csbi.wAttributes);
	}
	break;
    default: /* All */
	p.X = 0;
	p.Y = 0;
	while (p.Y <= csbi.dwSize.Y) {
	    clear_eol(conout, p, csbi.dwSize.X - 1, csbi.wAttributes);
	    p.Y ++;
	};
	p.X = 0;
	p.Y = 0;
	if (!SetConsoleCursorPosition(conout, p)) {
	    win32_maperr(GetLastError());
	    CloseHandle(conout);
	    uerror("netsys_clear_console/SetConsoleCursorPosition", Nothing);
	};
	width = csbi.srWindow.Right - csbi.srWindow.Left  + 1;
	height = csbi.srWindow.Bottom - csbi.srWindow.Top + 1;
	new_win.Left = 0;
	new_win.Right = width-1;
	new_win.Top = 0;
	new_win.Bottom = height-1;
	if (!SetConsoleWindowInfo(conout, TRUE, &new_win)) {
	    win32_maperr(GetLastError());
	    CloseHandle(conout);
	    uerror("netsys_clear_console/SetConsoleWindowInfo", Nothing);
	}
	break;
    };

    CloseHandle(conout);

    return Val_unit;
#else
    invalid_argument("netsys_clear_console");
#endif
}


CAMLprim value netsys_get_current_thread_id(value dummy) {
#ifdef _WIN32
    return caml_copy_int32(GetCurrentThreadId());
#else
    invalid_argument("netsys_get_current_thread_id");
#endif
}


#ifdef _WIN32
/* CancelSynchronousIo is first available in Vista */
typedef BOOL (WINAPI *cancel_io_func_t)(HANDLE);
static cancel_io_func_t cancel_io_func = NULL;
static int cancel_checked = FALSE;
#endif


CAMLprim value netsys_cancel_synchronous_io(value thread_id_val) {
#ifdef _WIN32
    DWORD thread_id;
    HANDLE thread;
    int code;

    if (!cancel_checked) {
	cancel_io_func = (cancel_io_func_t)
	    GetProcAddress(GetModuleHandle("kernel32"), "CancelSynchronousIo");
	cancel_checked = 1;
    };

    if (cancel_io_func != NULL) {
	thread_id = Int32_val(thread_id_val);
	thread = OpenThread(THREAD_TERMINATE,FALSE,thread_id);
	if (thread == NULL) {
	    win32_maperr(GetLastError());
	    uerror("netsys_cancel_synchronous_io/OpenThread", Nothing);
	}
	code = cancel_io_func(thread);
	if (!code) {
	    code = GetLastError();   
	    if (code != ERROR_NOT_FOUND) {  /* hide ERROR_NOT_FOUND */
		CloseHandle(thread);
		win32_maperr(GetLastError());
		uerror("netsys_cancel_synchronous_io/CancelSynchronousIo",
		       Nothing);
	    }
	};
	CloseHandle(thread);
    };

    return Val_unit;
#else
    invalid_argument("netsys_cancel_synchronous_io");
#endif
}





/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

/* The simple and probably bug-free version of [Unix.select] shipped
   with O'Caml 3.10
*/

#ifdef _WIN32
static void fdlist_to_fdset(value fdlist, fd_set *fdset)
{
  value l;
  FD_ZERO(fdset);
  for (l = fdlist; l != Val_int(0); l = Field(l, 1)) {
    FD_SET(Socket_val(Field(l, 0)), fdset);
  }
}
#endif

#ifdef _WIN32
static value fdset_to_fdlist(value fdlist, fd_set *fdset)
{
  value res = Val_int(0);
  value s = Val_int(0);
  Begin_roots3(fdlist, res, s)
    for (/*nothing*/; fdlist != Val_int(0); fdlist = Field(fdlist, 1)) {
      s = Field(fdlist, 0);
      if (FD_ISSET(Socket_val(s), fdset)) {
        value newres = alloc_small(2, 0);
        Field(newres, 0) = s;
        Field(newres, 1) = res;
        res = newres;
      }
    }
  End_roots();
  return res;
}
#endif

CAMLprim value netsys_real_select(value readfds, value writefds, 
				  value exceptfds, 
				  value timeout)
{
#ifdef _WIN32
  fd_set read, write, except;
  double tm;
  struct timeval tv;
  struct timeval * tvp;
  int retcode;
  value res;
  value read_list = Val_unit, write_list = Val_unit, except_list = Val_unit;
  DWORD err = 0;

  Begin_roots3 (readfds, writefds, exceptfds)
  Begin_roots3 (read_list, write_list, except_list)
    tm = Double_val(timeout);
    if (readfds == Val_int(0)
	&& writefds == Val_int(0)
	&& exceptfds == Val_int(0)) {
      if ( tm > 0.0 ) {
	enter_blocking_section();
	Sleep( (int)(tm * 1000));
	leave_blocking_section();
      }
      read_list = write_list = except_list = Val_int(0);
    } else {      
      fdlist_to_fdset(readfds, &read);
      fdlist_to_fdset(writefds, &write);
      fdlist_to_fdset(exceptfds, &except);
      if (tm < 0.0)
	tvp = (struct timeval *) NULL;
      else {
	tv.tv_sec = (int) tm;
	tv.tv_usec = (int) (1e6 * (tm - (int) tm));
	tvp = &tv;
      }
      enter_blocking_section();
      if (select(FD_SETSIZE, &read, &write, &except, tvp) == -1)
        err = WSAGetLastError();
      leave_blocking_section();
      if (err) {
	win32_maperr(err);
	uerror("select", Nothing);
      }
      read_list = fdset_to_fdlist(readfds, &read);
      write_list = fdset_to_fdlist(writefds, &write);
      except_list = fdset_to_fdlist(exceptfds, &except);
    }
    res = alloc_small(3, 0);
    Field(res, 0) = read_list;
    Field(res, 1) = write_list;
    Field(res, 2) = except_list;
  End_roots();
  End_roots();
  return res;
#else
    invalid_argument("netsys_real_select");
#endif
}
