;; counsel-ctags-tests.el --- unit tests for counsel-ctags -*- coding: utf-8 -*-

;; Author: Chen Bin <chenbin DOT sh AT gmail DOT com>

;;; License:

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

(require 'ert)
(require 'js)

(setq counsel-ctags-debug t)

(defun get-full-path (filename)
  "Get full path of FILENAME."
  (concat
   (if load-file-name (file-name-directory load-file-name) default-directory)
   filename))

(ert-deftest counsel-ctags-test-find-tag ()
  ;; one hello function in test.js
  ;; one hello function, one hello method and one test method in hello.js
  (let* (cands
         context
         (tags-file (get-full-path "tags.test")))
    ;; all tags across project, case insensitive, fuzzy match.
    ;; So "CHello" is also included
    (setq cands (counsel-ctags-extract-cands tags-file "hello" t))
    (should (eq (length cands) 6))

    ;; all tags across project, case sensitive
    (setq cands (counsel-ctags-extract-cands tags-file "hello" nil))
    (should (eq (length cands) 3))

    ;; one function named "test"
    (setq cands (counsel-ctags-extract-cands tags-file "test" nil))
    (should (eq (length cands) 1))))

(ert-deftest counsel-ctags-test-sort-cands-by-filename ()
  (let* (cands
         (tags-file (get-full-path "tags.test"))
         (f (get-full-path "test.js"))
         sorted-cands)
    (setq cands (counsel-ctags-extract-cands tags-file "hello" nil))
    (should (eq (length cands) 3))

    ;; at bottom
    (should (string-match "test.js" (counsel-ctags-get :file (nth 2 cands))))

    (setq sorted-cands (counsel-ctags-sort-candidates-maybe cands 3 f))

    ;; now it's on top after sorting
    (should (string-match "test.js" (counsel-ctags-get :file (nth 0 sorted-cands))))))

(ert-deftest counsel-ctags-test-tags-file-cache ()
  (let* (cands
         (tags-file (get-full-path "tags.test")))
    ;; clear cache
    (setq counsel-ctags-cache nil)
    (setq cands (counsel-ctags-extract-cands tags-file "hello" nil))
    (should (eq (length cands) 3))
    ;; cache is filled
    (should counsel-ctags-cache)
    (should (counsel-ctags-cache-content tags-file))))

(ert-deftest counsel-ctags-test-tag-history ()
  (let* (cands
         (tags-file (get-full-path "tags.test"))
         (dir (get-full-path "")))
    ;; clear history
    (setq counsel-ctags-tag-history nil)
    (setq cands (counsel-ctags-extract-cands tags-file "hello" nil))
    (should (eq (length cands) 3))
    ;; only add tag when it's accessed by user manually
    (should (not counsel-ctags-tag-history))
    (dolist (c cands) (counsel-ctags-remember c))
    (should counsel-ctags-tag-history)
    (should (eq (length counsel-ctags-tag-history) 3))))

(ert-run-tests-batch-and-exit)
