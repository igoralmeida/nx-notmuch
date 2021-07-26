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


