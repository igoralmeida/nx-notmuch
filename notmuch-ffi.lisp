;;;; This file is part of nx-notmuch.

;;;; nx-notmuch is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.

;;;; nx-notmuch is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.

;;;; You should have received a copy of the GNU General Public License
;;;; along with nx-notmuch.  If not, see <https://www.gnu.org/licenses/>.

(in-package :nx-notmuch)

;; definitions

(cffi:defctype dbmode :int)

;; utils

(cffi:defcfun "notmuch_status_to_string" :string
  (status :int))

;; database

(cffi:defcfun "notmuch_database_open" :int
  (path :string)
  (mode dbmode)
  (database :pointer))

(cffi:defcfun "notmuch_database_close" :int
  (database :pointer))

(cffi:defcfun "notmuch_database_get_path" :string
  (database :pointer))

(cffi:defcfun "notmuch_database_get_version" :int
  (database :pointer))

(cffi:defcfun "notmuch_database_get_all_tags" :pointer
  (database :pointer))

(cffi:defcfun "notmuch_database_get_config_list" :int
  (db :pointer)
  (prefix :string)
  (out :pointer))

;; tags iterator

(cffi:defcfun "notmuch_tags_valid" :bool
  (tags :pointer))

(cffi:defcfun "notmuch_tags_get" :string
  (tags :pointer))

(cffi:defcfun "notmuch_tags_move_to_next" :void
  (tags :pointer))

(cffi:defcfun "notmuch_tags_destroy" :void
  (tags :pointer))

;; config iterator

(cffi:defcfun "notmuch_config_list_valid" :bool
  (config-list :pointer))

(cffi:defcfun "notmuch_config_list_key" :string
  (config-list :pointer))

(cffi:defcfun "notmuch_config_list_value" :string
  (config-list :pointer))

(cffi:defcfun "notmuch_config_list_move_to_next" :void
  (config-list :pointer))

(cffi:defcfun "notmuch_config_list_destroy" :void
  (config-list :pointer))

;; query

(cffi:defcfun "notmuch_query_create" :pointer
  (database :pointer)
  (query-string :string))

(cffi:defcfun "notmuch_query_destroy" :void
  (query :pointer))

;; search

(cffi:defcfun "notmuch_query_search_threads" :int
  (query :pointer)
  (out :pointer))

;; threads iterator

(cffi:defcfun "notmuch_threads_valid" :bool
  (threads :pointer))

(cffi:defcfun "notmuch_threads_get" :pointer
  (threads :pointer))

(cffi:defcfun "notmuch_threads_move_to_next" :void
  (threads :pointer))

(cffi:defcfun "notmuch_threads_destroy" :void
  (threads :pointer))

;; thread

(cffi:defcfun "notmuch_thread_get_thread_id" :string
  (thread :pointer))

(cffi:defcfun "notmuch_thread_get_subject" :string
  (thread :pointer))

(cffi:defcfun "notmuch_thread_get_authors" :string
  (thread :pointer))

(cffi:defcfun "notmuch_thread_get_tags" :pointer
  (thread :pointer))

(cffi:defcfun "notmuch_thread_get_oldest_date" :long
  (thread :pointer))

(cffi:defcfun "notmuch_thread_get_newest_date" :long
  (thread :pointer))

;; commands and utils

(defun libnotmuch-collect-tags (tags-handle)
  "Given TAGS-HANDLE, collect all tags in a list."
  (loop while (notmuch-tags-valid tags-handle)
        collect (notmuch-tags-get tags-handle)
        do (notmuch-tags-move-to-next tags-handle)))

(defun libnotmuch-collect-threads (threads-handle)
  "Given THREADS-HANDLE, collect all threads in a list."
  (loop while (notmuch-threads-valid threads-handle)
        collect (let ((thread (notmuch-threads-get threads-handle)))
                  (list :thread (notmuch-thread-get-thread-id thread)
                        :subject (notmuch-thread-get-subject thread)
                        :authors (notmuch-thread-get-authors thread)
                        :date_relative (relative-date
                                        (notmuch-thread-get-newest-date thread)
                                        (local-time:timestamp-to-unix (local-time:now)))
                        :tags (libnotmuch-collect-tags (notmuch-thread-get-tags thread))))
        do (notmuch-threads-move-to-next threads-handle)))

(defun libnotmuch-search (search-string db-path)
  "Search using the libnotmuch FFI.

This is hard-coded to open the database at DB-PATH in read-only mode."
  (let* (db-open-status db-handle
         query-handle search-status
         threads-handle thread-list)
    (cffi:with-foreign-objects ((db-handle-pointer :pointer)
                                (search-handle-pointer :pointer))
      (setf db-open-status (notmuch-database-open db-path 0 db-handle-pointer))
      (when (eq db-open-status 0)
        (setf db-handle (cffi:mem-ref db-handle-pointer :pointer)
              query-handle (notmuch-query-create db-handle search-string)
              search-status (notmuch-query-search-threads
                             query-handle search-handle-pointer))

        (when (eq search-status 0)
          (setf threads-handle (cffi:mem-ref search-handle-pointer :pointer)
                thread-list (libnotmuch-collect-threads threads-handle))
          (notmuch-query-destroy query-handle)
          (unless (eq (notmuch-database-close db-handle) 0)
            (echo-warning "db close not successful"))
          thread-list)))))

(defun libnotmuch-lazy-search-begin (search-string db-path)
  "Set up a search using the libnotmuch FFI, returning its pointers.

The follow-up to this call would be using `libnotmuch-threads-for-each' with the
first element of the returned list.

When the search is no longer necessary, call `libnotmuch-lazy-search-end' to
tear everything down (and close the database)."
  (let* (db-open-status db-handle
         query-handle search-status
         threads-handle
         db-handle-pointer search-handle-pointer)

    (setf db-handle-pointer (cffi:foreign-alloc :pointer))
    (setf search-handle-pointer (cffi:foreign-alloc :pointer))

    (setf db-open-status (notmuch-database-open db-path 0 db-handle-pointer))
    (when (eq db-open-status 0)
      (setf db-handle (cffi:mem-ref db-handle-pointer :pointer)
            query-handle (notmuch-query-create db-handle search-string)
            search-status (notmuch-query-search-threads
                           query-handle search-handle-pointer))

      (when (eq search-status 0)
        (setf threads-handle (cffi:mem-ref search-handle-pointer :pointer))))

    (list threads-handle
          db-handle-pointer
          search-handle-pointer
          query-handle)))

(defun libnotmuch-lazy-search-end (objs)
  "Finalize search using the OBJS returned by `libnotmuch-lazy-search-begin'."
  (destructuring-bind (db-handle-pointer
                       search-handle-pointer
                       query-handle)
      (rest objs)

    (let ((db-handle (cffi:mem-ref db-handle-pointer :pointer)))
      (notmuch-query-destroy query-handle)
      (unless (eq (notmuch-database-close db-handle) 0)
        (echo-warning "db close not successful")))

    (cffi:foreign-free search-handle-pointer)
    (cffi:foreign-free db-handle-pointer)))

(defun thread-to-plist (thread-handle)
  "Fetch information from THREAD-HANDLE using libnotmuch."
  (list :thread (notmuch-thread-get-thread-id thread-handle)
        :subject (notmuch-thread-get-subject thread-handle)
        :authors (notmuch-thread-get-authors thread-handle)
        :date_relative (relative-date
                        (notmuch-thread-get-newest-date thread-handle)
                        (local-time:timestamp-to-unix (local-time:now)))
        :tags (libnotmuch-collect-tags (notmuch-thread-get-tags thread-handle))))

(defun libnotmuch-threads-for-each (f threads-handle)
  "Call function F on each of the threads accessible with THREADS-HANDLE."
  (loop while (notmuch-threads-valid threads-handle)
        do (let ((thread (notmuch-threads-get threads-handle)))
               (funcall f (thread-to-plist thread)))))

(defun libnotmuch-get-all-tags (db-path)
  "Get all tags in the database at DB-PATH using the libnotmuch FFI."
  (let* (db-open-status db-handle tags-handle tags)
    (cffi:with-foreign-objects ((db-handle-pointer :pointer))
      (setf db-open-status (notmuch-database-open db-path 0 db-handle-pointer))
      (when (eq db-open-status 0)
        (setf db-handle (cffi:mem-ref db-handle-pointer :pointer))
        (setf tags-handle (notmuch-database-get-all-tags db-handle))
        (setf tags (loop while (notmuch-tags-valid tags-handle)
                         collect (notmuch-tags-get tags-handle)
                         do (notmuch-tags-move-to-next tags-handle)))
        (notmuch-tags-destroy tags-handle)

        (unless (eq (notmuch-database-close db-handle) 0)
            (echo-warning "db close not successful"))
        tags))))

