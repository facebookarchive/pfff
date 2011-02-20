/* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Header lwt_unix
 * Copyright (C) 2010 Jérémie Dimino
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation, with linking exceptions;
 * either version 2.1 of the License, or (at your option) any later
 * version. See COPYING file for details.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 */

#ifndef __LWT_UNIX_H
#define __LWT_UNIX_H

#include <caml/mlvalues.h>
#include <caml/unixsupport.h>
#include <pthread.h>

/* Detect the target OS */
#if defined(_WIN32) || defined(_WIN64)
#  define LWT_ON_WINDOWS
#endif

/* The macro to get the file-descriptor from a value. */
#if defined(LWT_ON_WINDOWS)
#  define FD_val(value) win_CRT_fd_of_filedescr(value)
#else
#  define FD_val(value) Int_val(value)
#endif

/* Macro to extract a libev loop from a caml value. */
#define Ev_loop_val(value) *(struct ev_loop**)Data_custom_val(value)

/* +-----------------------------------------------------------------+
   | Utils                                                           |
   +-----------------------------------------------------------------+ */

/* Allocate the given amount of memory and abort the program if there
   is no free memory left. */
void *lwt_unix_malloc(size_t size);

/* Same as [strdup] and abort hte program if there is not memory
   left. */
char *lwt_unix_strdup(char *string);

/* Helper for allocating structures. */
#define lwt_unix_new(type) (type*)lwt_unix_malloc(sizeof(type))

/* Raise [Lwt_unix.Not_available]. */
void lwt_unix_not_available(char const *feature) Noreturn;

/* +-----------------------------------------------------------------+
   | Notifications                                                   |
   +-----------------------------------------------------------------+ */

/* Sends a notification for the given id. */
void lwt_unix_send_notification(int id);

/* +-----------------------------------------------------------------+
   | Threading                                                       |
   +-----------------------------------------------------------------+ */

/* Launch a thread in detached mode. */
pthread_t lwt_unix_launch_thread(void* (*start)(void*), void* data);

/* +-----------------------------------------------------------------+
   | Detached jobs                                                   |
   +-----------------------------------------------------------------+ */

/* How job are executed. */
enum lwt_unix_async_method {
  /* Synchronously. */
  LWT_UNIX_ASYNC_METHOD_NONE = 0,

  /* Asynchronously, on another thread. */
  LWT_UNIX_ASYNC_METHOD_DETACH = 1,

  /* Asynchronously, on the main thread, switcing to another thread if
     necessary. */
  LWT_UNIX_ASYNC_METHOD_SWITCH = 2
};

/* Type of job execution modes. */
typedef enum lwt_unix_async_method lwt_unix_async_method;

/* A job descriptor. */
struct lwt_unix_job {
  /* The next job in the queue. */
  struct lwt_unix_job *next;

  /* Id used to notify the main thread in case the job do not
     terminate immediatly. */
  int notification_id;

  /* The function to call to do the work. */
  void (*worker)(struct lwt_unix_job *job);

  /* Is the job terminated ? In case the job is canceled, it will
     always be 0. */
  int done;

  /* Is the main thread still waiting for the job ? */
  int fast;

  /* Mutex to protect access to [done] and [fast]. */
  pthread_mutex_t mutex;

  /* Thread running the job. */
  pthread_t thread;

  /* Whether the [thread] field has been initialized. */
  int thread_initialized;

  /* The async method in used by the job. */
  lwt_unix_async_method async_method;
};

/* Type of job descriptors. */
typedef struct lwt_unix_job* lwt_unix_job;

/* Type of worker functions. */
typedef void (*lwt_unix_job_worker)(lwt_unix_job job);

/* Allocate a caml custom value for the given job. */
value lwt_unix_alloc_job(lwt_unix_job job);

/* Free resourecs allocated for this job and free it. */
void lwt_unix_free_job(lwt_unix_job job);

/* Define not implement methods. */
#define LWT_UNIX_JOB_NOT_IMPLEMENTED(name)      \
  CAMLprim value lwt_unix_##name##_job()        \
  {                                             \
    caml_invalid_argument("not implemented");	\
  }                                             \
                                                \
  CAMLprim value lwt_unix_##name##_result()     \
  {                                             \
    caml_invalid_argument("not implemented");	\
  }                                             \
                                                \
  CAMLprim value lwt_unix_##name##_free()       \
  {                                             \
    caml_invalid_argument("not implemented");	\
  }


#endif /* __LWT_UNIX_H */
