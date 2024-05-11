;;; counsel-ctags.el ---  Fast code navigation using Universal Ctags  -*- lexical-binding: t -*-

;; Copyright (C) 2024 Chen Bin

;; Author: Chen Bin <chb_sh@hotmail.com>
;; URL: http://github.com/redguardtoo/counsel-ctags
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools, convenience
;; Version: 1.10.1

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;;  Configuration,
;;
;;   Universal Ctags should exist.
;;   Please note ONLY default output format of Universal Ctags is supported.
;;   Emacs etags and Exuberant Ctags are NOT supported.
;;
;; Usage,
;;
;;   `counsel-ctags-find-tag-at-point' to navigate.  This command will also
;;   run `counsel-ctags-scan-code' AUTOMATICALLY if tags file does not exist.
;;   It also calls `counsel-ctags-fallback-grep-function' if not tag is found.
;;
;;   Use `counsel-ctags-imenu-excluded-names' to exclude tags by name in imenu.
;;   Use `counsel-ctags-imenu-excluded-types' to exclude tags by type in imenu.
;;
;;   `counsel-ctags-scan-code' to create tags file
;;   `counsel-ctags-grep' to grep
;;   `counsel-ctags-grep-extra-arguments' has extra arguments for grep
;;   `counsel-ctags-grep-current-directory' to grep in current directory
;;   `counsel-ctags-recent-tag' to open recent tag
;;   `counsel-ctags-find-tag' to two steps tag matching use regular expression and filter
;;   `counsel-ctags-list-tag' to list all tags
;;   `counsel-ctags-update-tags-force' to update current tags file by force
;;   `counsel-ctags-ignore-config-file' specifies paths of ignore configuration files
;;   (".gitignore", ".hgignore", etc).  Path is either absolute or relative to the tags file.
;;   `counsel-ctags-universal-ctags-p' to detect if Universal Ctags is used.
;;   `counsel-ctags-exuberant-ctags-p' to detect if Exuberant Ctags is used.
;;   See documentation of `counsel-ctags-use-ripgrep-force' on using ripgrep.
;;   If it's not set, correct grep program is automatically detected.
;;
;; Tips,
;; - Use `pop-tag-mark' to jump back.
;;
;; - The grep program path on Native Windows Emacs uses either forward slash or
;;   backward slash.  Like "C:/rg.exe" or "C:\\\\rg.exe".
;;   If grep program path is added to environment variable PATH, you don't need
;;   worry about slash problem.
;;
;; - Add below code into "~/.emacs" to AUTOMATICALLY update tags file:
;;
;;   ;; Don't ask before reloading updated tags files
;;   (setq tags-revert-without-query t)
;;   ;; NO warning when loading large tag files
;;   (setq large-file-warning-threshold nil)
;;   (add-hook 'prog-mode-hook
;;     (lambda ()
;;       (add-hook 'after-save-hook
;;                 'counsel-ctags-virtual-update-tags 'append 'local)))
;;
;; - You can use ivy's exclusion patterns to filter candidates.
;;   For example, input "keyword1 !keyword2 keyword3" means:
;;   "(keyword1 and (not (or keyword2 keyword3)))"
;;
;; - `counsel-ctags-extra-tags-files' contains extra tags files to parse.
;;   Set it like,
;;     (setq counsel-ctags-extra-tags-files
;;           '("./tags" "/usr/include/tags" "$PROJ1/include/tags"))
;;
;;   Files in `counsel-ctags-extra-tags-files' should have symbols with absolute path only.
;;
;; - You can set up `counsel-ctags-ignore-directories' and `counsel-ctags-ignore-filenames',
;;   (with-eval-after-load 'counsel-ctags
;;      ;; counsel-ctags-ignore-directories does NOT support wildcast
;;      (push "build_clang" counsel-ctags-ignore-directories)
;;      (push "build_clang" counsel-ctags-ignore-directories)
;;      ;; counsel-ctags-ignore-filenames supports wildcast
;;      (push "TAGS" counsel-ctags-ignore-filenames)
;;      (push "*.json" counsel-ctags-ignore-filenames))
;;
;;  - Rust programming language is supported.
;;    The easiest setup is to use ".dir-locals.el".
;;   in root directory.  The content of .dir-locals.el" is as below,
;;
;;   ((nil . ((counsel-ctags-update-tags-backend . (lambda (src-dir) (shell-command "rusty-tags Emacs")))
;;            (counsel-ctags-tags-file-name . "rusty-tags.emacs"))))
;;
;;  - User could use `counsel-ctags-convert-grep-keyword' to customize grep keyword.
;;    Below setup enable `counsel-ctags-grep' to search Chinese using pinyinlib,
;;
;;    (unless (featurep 'pinyinlib) (require 'pinyinlib))
;;    (setq counsel-ctags-convert-grep-keyword
;;      (lambda (keyword)
;;        (if (and keyword (> (length keyword) 0))
;;            (pinyinlib-build-regexp-string keyword t)
;;          keyword)))
;;
;;  - `counsel-ctags-find-tag-name-function' finds tag name at point.  If it returns nil,
;;    `find-tag-default' is used.  `counsel-ctags-word-at-point' gets word at point.
;;
;;  - You can append extra content into tags file in `counsel-ctags-after-update-tags-hook'.
;;    The parameter of hook is full path of the tags file.
;;    `counsel-ctags-tag-line' and `counsel-ctags-append-to-tags-file' are helper functions
;;    to update tags file in the hook.
;;
;;  - The ignore files (.gitignore, etc) are automatically detected and append to ctags
;;    cli options as "--exclude="@/ignore/file/path".
;;    Set `counsel-ctags-ignore-config-files' to nil to turn off this feature.
;;
;;  - Grep result is sorted by string distance of current file path and candidate file path.
;;    The sorting happens in Emacs 27+.
;;    You can set `counsel-ctags-sort-grep-result-p' to nil to disable sorting.

;;  - Run `counsel-ctags-list-tag-in-current-file' to list tags in current file.
;;    You can also use native imenu with below setup,
;;      (setq imenu-create-index-function
;;            'counsel-ctags-imenu-default-create-index-function)
;;
;; See https://github.com/redguardtoo/counsel-ctags/ for more tips.

;;; Code:

(require 'xref nil t) ; xref is optional
(require 'cl-lib)
(require 'find-file)
(require 'ivy nil t)
(require 'counsel nil t) ; counsel => swiper => ivy
(require 'tramp nil t)
(require 'browse-url)

(defgroup counsel-ctags nil
  "Complete solution to use ctags."
  :group 'tools)

(defcustom counsel-ctags-browse-url-function 'browse-url-generic
  "The function to open url in tags file."
  :group 'counsel-ctags
  :type 'function)

(defcustom counsel-ctags-ignore-config-files
  '(".gitignore"
    ".hgignore"
    "~/.ignore")
  "Path of configuration file which specifies files that should ignore.
Path is either absolute path or relative to the tags file."
  :group 'counsel-ctags
  :type '(repeat string))

(defcustom counsel-ctags-command-to-scan-single-code-file nil
  "Shell Command to scan single file.
If it's nil, a command using ctags is automatically created."
  :group 'counsel-ctags
  :type 'string)

(defcustom counsel-ctags-extra-tags-files nil
  "List of extra tags files to load.  They are not updated automatically.

A typical format is

    (\"./tags\" \"/usr/include/tags\" \"$PROJECT/*/include/tags\")

Environment variables can be inserted between slashes (`/').
They will be replaced by their definition.  If a variable does
not exist, it is replaced (silently) with an empty string.

Symbol location inside tags file should use absolute path.
A CLI to create tags file:

  find /usr/include | ctags -L -"
  :group 'counsel-ctags
  :type '(repeat 'string))

(defcustom counsel-ctags-stop-auto-update-tags nil
  "If t, tags will not be updated automatically."
  :group 'counsel-ctags
  :type 'boolean)

(defcustom counsel-ctags-use-ripgrep-force nil
  "Force use ripgrep as grep program.
If rg is not in $PATH, then it need be defined in `counsel-ctags-grep-program'."
  :group 'counsel-ctags
  :type 'boolean)

(defcustom counsel-ctags-ripgrep-default-options
  ;; @see https://github.com/BurntSushi/ripgrep/issues/501
  ;; some shell will expand "/" to a complete file path.
  ;; so try to avoid "/" in shell
  (format "-n -M 1024 --no-heading --color never -s %s"
          (if (eq system-type 'windows-nt) "--path-separator \"\x2f\"" ""))
  "Default options passed to ripgrep command line program."
  :group 'counsel-ctags
  :type 'boolean)

(defcustom counsel-ctags-grep-extra-arguments ""
  "Extra arguments passed to grep program."
  :group 'counsel-ctags
  :type 'string)

(defcustom counsel-ctags-convert-grep-keyword 'identity
  "Convert keyword to grep to new regex to feed into grep program."
  :group 'counsel-ctags
  :type 'function)

(defcustom counsel-ctags-fallback-grep-function #'counsel-ctags-grep
  "The fallback grep function if tag can't be found at first.
Hope grep can find something.

Below parameters is passed to the function.
The parameter \"keyword\" is the search keyword.
The parameter \"hint\" is the hint for grep ui.
The parameter \"root\" is the project root directory."
  :group 'counsel-ctags
  :type 'function)

(defcustom counsel-ctags-can-skip-project-root nil
  "If t, scanning project root is optional."
  :group 'counsel-ctags
  :type 'boolean)

(defcustom counsel-ctags-find-tag-name-function 'counsel-ctags-find-tag-name-default
  "The function to use to find tag name at point.
It should be a function that takes no arguments and returns an string.
If it returns nil, the `find-tag-default' is used.

The function `counsel-ctags-word-at-point' could be used find word at point.
The definition of word is customized by the user."
  :group 'counsel-ctags
  :type 'function)

(defcustom counsel-ctags-major-modes-to-strip-default-tag-name
  '(org-mode
    markdown-mode)
  "Major mode where default tag name need be stripped.
It's used by `counsel-ctags-find-tag-name-default'."
  :group 'counsel-ctags
  :type '(repeat 'sexp))

(defcustom counsel-ctags-ignore-directories
  '(;; VCS
    ".git"
    ".svn"
    ".cvs"
    ".bzr"
    ".hg"
    ;; project misc
    "bin"
    "dist"
    "fonts"
    "images"
    ;; Mac
    ".DS_Store"
    ;; html/javascript/css
    ".npm"
    ".tmp" ; TypeScript
    ".sass-cache" ; SCSS/SASS
    ".idea"
    "node_modules"
    "bower_components"
    ;; python
    ".tox"
    ;; vscode
    ".vscode"
    ;; emacs
    ".cask")
  "Ignore directory names."
  :group 'counsel-ctags
  :type '(repeat 'string))

(defcustom counsel-ctags-ignore-filenames
  '(;; VCS
    ;; project misc
    "*.log"
    ;; rusty-tags
    "rusty-tags.vim"
    "rusty-tags.emacs"
    ;; Ctags
    "tags"
    "TAGS"
    ;; compressed
    "*.tgz"
    "*.gz"
    "*.xz"
    "*.zip"
    "*.tar"
    "*.rar"
    ;; Global/Cscope
    "GTAGS"
    "GPATH"
    "GRTAGS"
    "cscope.files"
    ;; html/javascript/css
    "*bundle.js"
    "*min.js"
    "*min.css"
    ;; Images
    "*.png"
    "*.jpg"
    "*.jpeg"
    "*.gif"
    "*.bmp"
    "*.tiff"
    "*.ico"
    ;; documents
    "*.doc"
    "*.docx"
    "*.xls"
    "*.ppt"
    "*.pdf"
    "*.odt"
    ;; C/C++
    ".clang-format"
    "*.obj"
    "*.so"
    "*.o"
    "*.a"
    "*.ifso"
    "*.tbd"
    "*.dylib"
    "*.lib"
    "*.d"
    "*.dll"
    "*.exe"
    ;; Java
    ".metadata*"
    "*.class"
    "*.war"
    "*.jar"
    ;; Emacs/Vim
    "*flymake"
    "#*#"
    ".#*"
    "*.swp"
    "*~"
    "*.elc"
    ;; Python
    "*.pyc")
  "Ignore file names.  Wildcast is supported."
  :group 'counsel-ctags
  :type '(repeat 'string))

(defcustom counsel-ctags-project-file '("tags" ".svn" ".hg" ".git")
  "The file/directory used to locate project root directory.
You can set up it in \".dir-locals.el\"."
  :group 'counsel-ctags
  :type '(repeat 'string))

(defcustom counsel-ctags-project-root nil
  "Project root directory.  The directory automatically detects if it's nil."
  :group 'counsel-ctags
  :type 'string)

(defcustom counsel-ctags-tags-file-name "tags"
  "Tags file name."
  :group 'counsel-ctags
  :type 'string)

(defcustom counsel-ctags-imenu-excluded-names
  '("this"
    "if"
    "unless"
    "import"
    "const"
    "public"
    "static"
    "private"
    "for"
    "while"
    "export"
    "declare"
    "let")
  "Some imenu items should be excluded by name."
  :group 'counsel-ctags
  :type '(repeat 'string))

(defcustom counsel-ctags-imenu-excluded-types
  '("variable"
    "constant")
  "Some imenu items should be excluded by type.
Run \"ctags -x some-file\" to see the type in second column of output."
  :group 'counsel-ctags
  :type '(repeat 'string))

(defcustom counsel-ctags-candidates-sorted-p t
  "If t, sort candidates by file/directory Levenshtein distance."
  :group 'counsel-ctags
  :type 'integer)

(defcustom counsel-ctags-sort-grep-result-p t
  "Sort grep result by string distance."
  :group 'counsel-ctags
  :type 'boolean)

(defcustom counsel-ctags-max-file-size 512
  "Ignore files bigger than `counsel-ctags-max-file-size' kilobytes.
This option is ignored if GNU find is not installed."
  :group 'counsel-ctags
  :type 'integer)

(defcustom counsel-ctags-after-update-tags-hook nil
  "Hook after tags file is actually updated.
The parameter of hook is full path of the tags file."
  :group 'counsel-ctags
  :type 'hook)

(defcustom counsel-ctags-org-property-name-for-grepping
  "GREP_PROJECT_ROOT"
  "Org node property name for get grepping project root."
  :group 'counsel-ctags
  :type 'string)

(defcustom counsel-ctags-org-extract-project-root-from-node-p
  t
  "Extract project root directory from org node."
  :group 'counsel-ctags
  :type 'boolean)

(defcustom counsel-ctags-update-interval 300
  "The interval (seconds) to update tags file.
Used by `counsel-ctags-virtual-update-tags'.
Default value is 300 seconds."
  :group 'counsel-ctags
  :type 'integer)

(defcustom counsel-ctags-ctags-program nil
  "Ctags Program.  Ctags is automatically detected if it's nil.
You can set it to the full path of the executable."
  :group 'counsel-ctags
  :type 'string)

(defcustom counsel-ctags-grep-program nil
  "Grep program.  Program is automatically detected if it's nil.
You can set it to the full path of the executable."
  :group 'counsel-ctags
  :type 'string)

(defcustom counsel-ctags-quiet-when-updating-tags t
  "Be quiet when updating tags."
  :group 'counsel-ctags
  :type 'boolean)

(defcustom counsel-ctags-update-tags-backend
  'counsel-ctags-scan-dir-internal
  "A user-defined function to update tags file during auto-updating.
The function has same parameters as `counsel-ctags-scan-dir-internal'."
  :group 'counsel-ctags
  :type 'sexp)

(defconst counsel-ctags-no-project-msg
  "No project found.  You can create tags file using `counsel-ctags-scan-code'.
So we don't need the project root at all.
Or you can set up `counsel-ctags-project-root'."
  "Message to display when no project is found.")

(defvar counsel-ctags-debug nil "Enable debug mode.")

;; Timer to run auto-update tags file
(defvar counsel-ctags-timer nil "Internal timer.")

(defvar counsel-ctags-keyword nil "The keyword to grep.")

(defvar counsel-ctags-opts-cache '() "Grep CLI options cache.")

(defvar counsel-ctags-tag-history nil "History of tag names.")

(defvar counsel-ctags-tags-file-history nil
  "Tags files history.  Recently accessed file is at the top of history.
The file is also used by tags file auto-update process.")

(defvar counsel-ctags-find-tag-candidates nil "Find tag candidate.")

(defvar counsel-ctags-cache nil "Cache of multiple tags files.")

(defvar counsel-ctags-last-tagname-at-point nil
  "Last tagname queried at point.")

(declare-function outline-up-heading "outline")
(declare-function org-entry-get "outline")

(defmacro counsel-ctags-get (property item)
  "Get PROPERTY of ITEM."
  `(plist-get (nth 1 ,item) ,property))

(defun counsel-ctags-org-entry-get-project-root ()
  "Get org property from current node or parent node recursively."
  (when (and (derived-mode-p 'org-mode)
             counsel-ctags-org-extract-project-root-from-node-p)
    (unless (featurep 'org) (require 'org))
    (unless (featurep 'outline) (require 'outline))
    (let* ((pos (point))
           (prop-name counsel-ctags-org-property-name-for-grepping)
           (rlt (org-entry-get pos prop-name))
           (loop t)
           old-pos)

      (save-excursion
        (unless rlt
          (setq old-pos (point))
          (condition-case nil (outline-up-heading 1) (error nil))
          (while loop
            (cond
             ((or (setq rlt (org-entry-get (point) prop-name))
                  (eq (point) old-pos))
              (setq loop nil))
             (t
              (setq old-pos (point))
              (condition-case nil (outline-up-heading 1) (error nil)))))
          (goto-char pos))
        rlt))))

(defun counsel-ctags-win-path (executable-name drive)
  "Guess EXECUTABLE-NAME's full path in Cygwin on DRIVE."
  (let* ((path (concat drive ":\\\\cygwin64\\\\bin\\\\" executable-name ".exe")))
    (if (file-exists-p path) path)))

;;;###autoload
(defun counsel-ctags-guess-program (executable-name)
  "Guess path from its EXECUTABLE-NAME on Windows.
Return nil if it's not found."
  (cond
   ((file-remote-p default-directory)
    ;; Assume remote server has already added EXE into $PATH!
    executable-name)
   ((eq system-type 'windows-nt)
    (or (counsel-ctags-win-path executable-name "c")
        (counsel-ctags-win-path executable-name "d")
        (counsel-ctags-win-path executable-name "e")
        (counsel-ctags-win-path executable-name "f")
        (counsel-ctags-win-path executable-name "g")
        (counsel-ctags-win-path executable-name "h")
        executable-name))
   (t
    (if (executable-find executable-name) (executable-find executable-name)))))

;;;###autoload
(defun counsel-ctags-version ()
  "Return version."
  (message "1.10.1"))

;;;###autoload
(defun counsel-ctags-get-hostname ()
  "Reliable way to get current hostname.
`(getenv \"HOSTNAME\")' won't work because $HOSTNAME is NOT an
 environment variable.
`system-name' won't work because /etc/hosts could be modified"
  (with-temp-buffer
    (shell-command "hostname" t)
    (goto-char (point-max))
    (delete-char -1)
    (buffer-string)))

(defun counsel-ctags-get-tags-file-path (dir)
  "Get full path of tags file from DIR."
  (and dir (expand-file-name (concat (file-name-as-directory dir)
                                  counsel-ctags-tags-file-name))))

(defun counsel-ctags-locate-tags-file ()
  "Find tags file: Search `counsel-ctags-tags-file-history' and parent directories."
  (counsel-ctags-get-tags-file-path (locate-dominating-file default-directory
                                                            counsel-ctags-tags-file-name)))

(defun counsel-ctags-tags-file-directory ()
  "Directory of tags file."
  (let* ((f (counsel-ctags-locate-tags-file)))
    (if f (file-name-directory (expand-file-name f)))))

(defun counsel-ctags-locate-project ()
  "Return the root of the project."
  (let* ((tags-dir (if (listp counsel-ctags-project-file)
                       (cl-some (apply-partially 'locate-dominating-file
                                                 default-directory)
                                counsel-ctags-project-file)
                     (locate-dominating-file default-directory
                                             counsel-ctags-project-file)))
         (project-root (or counsel-ctags-project-root
                           (and tags-dir (file-name-as-directory tags-dir)))))
    (or project-root
        (progn (message counsel-ctags-no-project-msg)
               nil))))

(defun counsel-ctags-add-tags-file-to-history (tags-file)
  "Add TAGS-FILE to the top of `counsel-ctags-tags-file-history'."
  (let* ((file (expand-file-name tags-file)))
    (setq counsel-ctags-tags-file-history
          (delq nil (mapcar
                     (lambda (s)
                       (unless (string= file (expand-file-name s)) s))
                     counsel-ctags-tags-file-history)))
    (push tags-file counsel-ctags-tags-file-history)))

;;;###autoload
(defun counsel-ctags-async-shell-command (command tags-file)
  "Execute string COMMAND and create TAGS-FILE asynchronously."
  (let* ((proc (start-file-process "Shell" nil shell-file-name shell-command-switch command)))
    (set-process-sentinel
     proc
     `(lambda (process signal)
        (let* ((status (process-status process)))
          (when (memq status '(exit signal))
            (cond
             ((string= (substring signal 0 -1) "finished")
              (let* ((cmd (car (cdr (cdr (process-command process))))))
                (if counsel-ctags-debug (message "`%s` executed." cmd))
                ;; If tramp exists and file is remote, clear file cache
                (when (and (fboundp 'tramp-cleanup-this-connection)
                           ,tags-file
                           (file-remote-p ,tags-file))
                  (tramp-cleanup-this-connection))
                ;; reload tags-file
                (when (and ,tags-file (file-exists-p ,tags-file))
                  (run-hook-with-args 'counsel-ctags-after-update-tags-hook ,tags-file)
                  (message "Tags file %s was created." ,tags-file))))
             (t
              (message "Failed to create tags file. Error=%s CLI=%s"
                       signal
                       ,command)))))))))

(defun counsel-ctags-dir-pattern (dir)
  "Trim * from DIR."
  (setq dir (replace-regexp-in-string "[*/]*\\'" "" dir))
  (setq dir (replace-regexp-in-string "\\`[*]*" "" dir))
  dir)


(defun counsel-ctags-emacs-bin-path ()
  "Get Emacs binary path."
  (let* ((emacs-executable (file-name-directory (expand-file-name invocation-name
                                                                  invocation-directory))))
    (replace-regexp-in-string "/" "\\\\" emacs-executable)))

(defun counsel-ctags--ctags--info (ctags-program)
  "Get CTAGS-PROGRAM information."
  (shell-command-to-string (concat ctags-program " --version")))

;;;###autoload
(defun counsel-ctags-exuberant-ctags-p (ctags-program)
  "If CTAGS-PROGRAM is Exuberant Ctags."
  (let* ((cmd-output (counsel-ctags--ctags--info ctags-program)))
    (and (not (string-match-p "Universal Ctags" cmd-output))
         (string-match-p "Exuberant Ctags" cmd-output))))

;;;###autoload
(defun counsel-ctags-universal-ctags-p (ctags-program)
  "If CTAGS-PROGRAM is Universal Ctags."
  (and (executable-find ctags-program)
       (not (counsel-ctags-exuberant-ctags-p ctags-program))))

(defun counsel-ctags-valid-ctags (ctags-program)
  "If CTAGS-PROGRAM is Ctags return the program.
If it's Emacs etags return nil."
  (when ctags-program
    (let* ((cmd-output (counsel-ctags--ctags--info ctags-program)))
      (unless (string-match-p " ETAGS.README" cmd-output)
        ctags-program))))

(defun counsel-ctags-languages (ctags-program)
  "List languages CTAGS-PROGRAM supports."
  (let* ((cmd (concat ctags-program " --list-languages")))
    (split-string (shell-command-to-string cmd) "\n")))

(defun counsel-ctags-convert-config (config program)
  "Convert CONFIG of PROGRAM into Universal Ctags format."
  (let* ((rlt config)
         (langs (counsel-ctags-languages program))
         ch
         regex)
    (dolist (lang langs)
      (when (not (string= "" lang))
        (setq ch (substring-no-properties lang 0 1))
        (setq regex (format "--langdef=[%s%s]%s *$"
                            ch
                            (downcase ch)
                            (substring-no-properties lang 1)))
        (setq rlt (replace-regexp-in-string regex "" rlt))))
    rlt))

(defun counsel-ctags-ctags-ignore-config ()
  "Specify ignore configuration file (.gitignore, for example) for Ctags."
  (let* (rlt configs filename)
    (dolist (f counsel-ctags-ignore-config-files)
      (when (file-exists-p (setq filename (expand-file-name f)))
        (push (file-local-name filename) configs)))
    (setq rlt (mapconcat (lambda (c) (format "--exclude=\"@%s\"" c)) configs " "))
    (when counsel-ctags-debug
        (message "counsel-ctags-ctags-ignore-config returns %s" rlt))
    rlt))

(defun counsel-ctags-get-scan-command (ctags-program &optional code-file)
  "Create command for CTAGS-PROGRAM.
If CODE-FILE is a real file, the command scans it and output to stdout."
  (let* ((cmd ""))
    (cond
     ;; Use ctags only
     (ctags-program
      (setq cmd
            (format "%s %s %s %s %s -R %s"
                    ctags-program
                    (mapconcat (lambda (p)
                                 (format "--exclude=\"*/%s/*\" --exclude=\"%s/*\""
                                         (counsel-ctags-dir-pattern p)
                                         (counsel-ctags-dir-pattern p)))
                               counsel-ctags-ignore-directories " ")
                    (mapconcat (lambda (p)
                                 (format "--exclude=\"%s\"" p))
                               counsel-ctags-ignore-filenames " ")
                    (counsel-ctags-ctags-ignore-config)
                    ;; print a tabular, human-readable cross reference
                    ;; --<my-lang>-kinds=f still accept all user defined regex
                    ;; so we have to filter in Emacs Lisp
                    (if code-file "-x -w" "")
                    (if code-file (format "\"%s\"" code-file) ""))))

     (t
      (message "You need install Universal Ctags at first.")))
    (when counsel-ctags-debug
      (message "counsel-ctags-get-scan-command called => ctags-program=%s cmd=%s"
               ctags-program cmd))
    cmd))

;;;###autoload
(defun counsel-ctags-scan-dir-internal (src-dir)
  "Create tags file from SRC-DIR."
  ;; TODO save the ctags-opts into hash
  (let* ((ctags-program (or counsel-ctags-ctags-program
                            (counsel-ctags-valid-ctags
                             (counsel-ctags-guess-program "ctags"))))
         (default-directory src-dir)
         ;; if both find and ctags exist, use both
         ;; if only ctags exists, use ctags
         ;; run find&ctags to create TAGS, `-print` is important option to filter correctly
         (cmd (counsel-ctags-get-scan-command ctags-program))
         (tags-file (counsel-ctags-get-tags-file-path src-dir)))
    (unless ctags-program
      (error "Please install Exuberant Ctags or Universal Ctags before running this program!"))
    (when counsel-ctags-debug
      (message "counsel-ctags-scan-dir-internal called => src-dir=%s" src-dir)
      (message "default-directory=%s cmd=%s" default-directory cmd))
    ;; always update cli options
    (message "%s at %s" (if counsel-ctags-debug cmd "Scan") default-directory)
    (counsel-ctags-async-shell-command cmd tags-file)))

(defun counsel-ctags-toggle-auto-update-tags ()
  "Stop/Start tags auto update."
  (interactive)
  (if (setq counsel-ctags-stop-auto-update-tags
            (not counsel-ctags-stop-auto-update-tags))
      (message "Tags is NOT automatically updated any more.")
    (message "Tags will be automatically updated.")))

(defun counsel-ctags-scan-dir (src-dir)
  "Create tags file from SRC-DIR."
  (if counsel-ctags-debug (message "counsel-ctags-scan-dir called => %s" src-dir))
  (cond
   (counsel-ctags-stop-auto-update-tags
    ;; do nothing
    )
   (t
    (funcall counsel-ctags-update-tags-backend src-dir))))

;;;###autoload
(defun counsel-ctags-directory-p (regex)
  "Does directory of current file match REGEX?"
  (let* ((case-fold-search nil)
         (dir (or (when buffer-file-name
                    (file-name-directory buffer-file-name))
                  ;; buffer is created in real time
                  default-directory
                  "")))
    (string-match-p regex dir)))

;;;###autoload
(defun counsel-ctags-filename-p (regex)
  "Does current file match REGEX?"
  (let* ((case-fold-search nil)
         (file (or buffer-file-name default-directory "")))
    (string-match-p regex file)))

(defun counsel-ctags-read-internal (file)
  "Read content of FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun counsel-ctags-write-internal (content file)
  "Write CONTENT into FILE."
  (write-region content nil file))

(defun counsel-ctags-read-file (file)
  "Return FILE content with child files included."
  (let* ((raw-content (counsel-ctags-read-internal file))
         (start 0)
         (re "^\\([^,]+\\),include$")
         included
         (extra-content ""))
    (while (setq start (string-match re raw-content start))
      (when (file-exists-p (setq included (match-string 1 raw-content)))
        (setq extra-content (concat extra-content
                                    "\n"
                                    (counsel-ctags-read-internal included))))
      (setq start (+ start (length included))))
    (concat raw-content extra-content)))

(defun counsel-ctags--strip-path (path strip-count)
  "Strip PATH with STRIP-COUNT."
  (let* ((i (1- (length path))))
    (while (and (> strip-count 0)
                (> i 0))
      (when (= (aref path i) ?/)
        (setq strip-count (1- strip-count)))
      (setq i (1- i)))
    (if (= 0 strip-count) (substring path (+ 1 i))
      path)))

(defun counsel-ctags-sort-candidates-maybe (cands strip-count current-file)
  "Sort CANDS by string distance.
STRIP-COUNT strips the string before calculating distance.
CURRENT-FILE is used to compare with candidate path."
  (when counsel-ctags-debug
    (message "counsel-ctags-sort-candidates-maybe called"))

  (let* ((ref (and current-file (counsel-ctags--strip-path current-file strip-count))))
    (cond
     ;; don't sort candidates if `current-file' is nil
     ((or (not ref)
          (not counsel-ctags-candidates-sorted-p))
      cands)

     (t
      (sort cands
            `(lambda (item1 item2)
               (let* ((a (counsel-ctags--strip-path (expand-file-name (counsel-ctags-get :file item1)) ,strip-count))
                      (b (counsel-ctags--strip-path (expand-file-name (counsel-ctags-get :file item2)) ,strip-count)))
                 (< (string-distance a ,ref t)
                    (string-distance b ,ref t)))))))))


(defun counsel-ctags-cache-invalidate (tags-file)
  "Invalidate the cache of TAGS-FILE."
  (plist-put counsel-ctags-cache (intern tags-file) nil))

(defun counsel-ctags-cache-content (tags-file)
  "Read cache using TAGS-FILE as key."
  (let* ((info (plist-get counsel-ctags-cache (intern tags-file))))
    (plist-get info :content)))

(defun counsel-ctags-cache-filesize (tags-file)
  "Read cache using TAGS-FILE as key."
  (let* ((info (plist-get counsel-ctags-cache (intern tags-file))))
    (or (plist-get info :filesize) 0)))

(defmacro counsel-ctags-put (key value dictionary)
  "Add KEY VALUE pair into DICTIONARY."
  `(setq ,dictionary (plist-put ,dictionary ,key ,value)))

(defun counsel-ctags-build-cand (info)
  "Build tag candidate from INFO.
If SHOW-ONLY-TEXT is t, the candidate shows only text."
  (let* ((head (format "%s: %s => %s"
                       (plist-get info :file)
                       (plist-get info :tagname)
                       (plist-get info :code-line))))
    (list head info)))

(defmacro counsel-ctags-push-one-candidate (cands tagname-re bound root-dir)
  "Push new candidate into CANDS.
Use TAGNAME-RE to search in current buffer with BOUND.
ROOT-DIR is the the directory of tags file."
    `(cond
      ((re-search-forward ,tagname-re ,bound t)
       (let* ((tagname (match-string-no-properties 1))
              (file (match-string-no-properties 2))
              (other (split-string (match-string-no-properties 3) "\\$/;\""))
              (code-line (nth 0 other))
              (non-code-line (split-string (string-trim (nth 1 other)) "\t"))
              (type (car non-code-line))
              (misc (cdr non-code-line))
              (cand (list :tagname tagname
                          :file file ; relative path
                          :code-line code-line
                          :type type
                          :misc misc
                          :root-dir ,root-dir)))
         (push (counsel-ctags-build-cand cand) ,cands))
       t)

      (t
       ;; need push cursor forward
       (end-of-line)
       nil)))

(defmacro counsel-ctags-scan-string (str tagname-re case-sensitive &rest body)
  "Scan STR using TAGNAME-RE and CASE-SENSITIVE and call BODY to push results."
  `(with-temp-buffer
    (insert ,str)
    ;; Not sure why `modify-syntax-entry' is used
    ;; Code is from https://www.emacswiki.org/emacs/etags-select.el
    (modify-syntax-entry ?_ "w")
    (goto-char (point-min))
    (let* ((case-fold-search ,case-sensitive))
      ;; normal tag search algorithm
      (while (re-search-forward ,tagname-re nil t)
        ,@body))
    ;; clean up, copied from "etags-select.el"
    (modify-syntax-entry ?_ "_")))


(defun counsel-ctags-search-regex (tagname)
  "Get regex to search TAGNAME which could be nil."
  (concat "^\\("
          (if tagname (regexp-quote tagname) "[^\t\r\n]+")
          "\\)\t"
          ;; file name
          "\\([^\t\r\n]+\\)\t"
          ;; other stuff, more analysis needed
          "/\\^\\([^\r\n]+\\)$"))

(defun counsel-ctags-extract-cands (tags-file tagname fuzzy)
  "Parse TAGS-FILE to find occurrences of TAGNAME using FUZZY algorithm."
  (let* ((root-dir (file-name-directory tags-file))
         (tagname-re (counsel-ctags-search-regex (unless fuzzy tagname)))
         cands
         file-size
         file-content)
    (when counsel-ctags-debug
      (message "counsel-ctags-extract-cands called. tags-file=%s cached-file-size=%s tags-file-size=%s"
               tags-file
               (counsel-ctags-cache-filesize tags-file)
               (nth 7 (file-attributes tags-file))))
    ;; ONLY when the checksum (file size) is different from the physical file size,
    ;; update cache by reading from physical file.
    ;; Not precise but acceptable algorithm.
    (when (and tags-file (file-exists-p tags-file)
               ;; TAGS file is smaller when being created.
               ;; Do NOT load incomplete tags file
               (< (counsel-ctags-cache-filesize tags-file)
                  (setq file-size (nth 7 (file-attributes tags-file)))))
      (when counsel-ctags-debug
        (message "Read file .... %s %s" (counsel-ctags-cache-filesize tags-file) file-size))
      (counsel-ctags-put (intern tags-file)
                         (list :content
                               (counsel-ctags-read-file tags-file)
                               :filesize
                               file-size)
                         counsel-ctags-cache))

    ;; Get better performance by scan from beginning to end.
    (when counsel-ctags-debug
      (message "counsel-ctags-extract-cands called. tags-file=%s tagname=%s" tags-file tagname))

    (when (and tags-file
               (setq file-content (counsel-ctags-cache-content tags-file)))
      (counsel-ctags-scan-string file-content
                                 tagname
                                 fuzzy
                                 (progn
                                   (beginning-of-line)
                                   (counsel-ctags-push-one-candidate cands
                                                                     tagname-re
                                                                     (line-end-position)
                                                                     root-dir)
                                   )))
    (and cands (nreverse cands))))

(defun counsel-ctags-collect-cands (tagname fuzzy current-file &optional dir)
  "Find TAGNAME using FUZZY algorithm in CURRENT-FILE of DIR."
  (let* (rlt
         (force-tags-file (and dir
                               (file-exists-p (counsel-ctags-get-tags-file-path dir))
                               (counsel-ctags-get-tags-file-path dir)))
         (tags-file (or force-tags-file
                        (counsel-ctags-locate-tags-file)))
         (cands (and tags-file (counsel-ctags-extract-cands tags-file
                                                            tagname
                                                            fuzzy))))

    (when counsel-ctags-debug
      (message "counsel-ctags-collect-cands called. tags-file=%s cands=%s" tags-file cands))

    ;; current-file is used to calculated string distance.
    (setq rlt (counsel-ctags-sort-candidates-maybe cands 3 current-file))

    (when counsel-ctags-extra-tags-files
      ;; don't sort candidate from 3rd party libraries
      (dolist (file (ff-list-replace-env-vars counsel-ctags-extra-tags-files))
        (when counsel-ctags-debug
          (message "load %s in %s" file counsel-ctags-extra-tags-files))
        (when (setq cands (counsel-ctags-extract-cands file
                                                       tagname
                                                       fuzzy))
          ;; don't bother sorting candidates from third party tags file
          (setq rlt (append rlt cands)))))

    rlt))

(defun counsel-ctags-regexp-quote(s)
  "Encode S."
  ;; encode "{}[]"
  (setq s (replace-regexp-in-string "\"" "\\\\\"" s))
  (setq s (replace-regexp-in-string "\\?" "\\\\\?" s))
  (setq s (replace-regexp-in-string "\\$" "\\\\x24" s))
  (setq s (replace-regexp-in-string "\\*" "\\\\\*" s))
  (setq s (replace-regexp-in-string "\\." "\\\\\." s))
  (setq s (replace-regexp-in-string "\\[" "\\\\\[" s))
  (setq s (replace-regexp-in-string "\\]" "\\\\\]" s))
  (setq s (replace-regexp-in-string "-" "\\\\-" s))
  ;; perl-regex support non-ASCII characters
  ;; Turn on `-P` from `git grep' and `grep'
  ;; the_silver_searcher and ripgrep need no setup
  (setq s (replace-regexp-in-string "{" "\\\\{" s))
  (setq s (replace-regexp-in-string "}" "\\\\}" s))
  s)

(defun counsel-ctags-selected-str ()
  "Get selected string.  Suppose plain text instead regex in selected text.
So we need *encode* the string."
  (when (region-active-p)
    (counsel-ctags-regexp-quote (buffer-substring-no-properties (region-beginning)
                                                                (region-end)))))

(defun counsel-ctags-tagname-at-point ()
  "Get tag name at point."
  (setq counsel-ctags-last-tagname-at-point
        (or (counsel-ctags-selected-str)
            (funcall counsel-ctags-find-tag-name-function))))

(defun counsel-ctags-forward-line (lnum)
  "Forward LNUM lines."
  (setq lnum (string-to-number lnum))
  (when (and lnum (> lnum 0))
    (goto-char (point-min))
    (forward-line (1- lnum))))

;;;###autoload
(defun counsel-ctags-push-marker-stack ()
  "Save current position."
  ;; un-select region
  (let* ((mark (point-marker)))
    (when (region-active-p) (pop-mark))
    ;; save current position into evil jump list
    ;; so user can press "C-o" to jump back
    (when (fboundp 'evil-set-jump) (evil-set-jump))
    ;; flash
    (cond
     ((fboundp 'xref-push-marker-stack)
      (xref-push-marker-stack mark))
     ((boundp 'find-tag-marker-ring)
      (ring-insert find-tag-marker-ring mark)))))

(defun counsel-ctags-open-file-api (item dir &optional tagname)
  "Open ITEM while `default-directory' is DIR.
Focus on TAGNAME if it's not nil."
  (when counsel-ctags-debug
    (message "counsel-ctags-open-file-api called => %s" item))
  ;; jump
  (let* ((file (counsel-ctags-get :file item))
         (code-line (counsel-ctags-get :code-line item))
         ;; always calculate path relative to TAGS
         (default-directory dir))

    (when counsel-ctags-debug
      (message "counsel-ctags-open-file-api called => dir=%s, code-line=%s, file=%s" dir code-line file))

    ;; item's format is like '~/proj1/ab.el:39: (defun hello() )'
    (counsel-ctags-push-marker-stack)

    (cond
     ;; file is actually a url template
     ((string-match "^https?://" file)
      (funcall counsel-ctags-browse-url-function (format file tagname)))

     (t
      ;; open file, go to certain line
      (find-file file)
      (counsel-ctags-forward-line code-line)

      ;; move focus to the tagname
      (beginning-of-line)
      ;; search tagname in current line might fail
      ;; maybe tags files is updated yet
      (when (and tagname
                 ;; focus on the tag if possible
                 (re-search-forward tagname (line-end-position) t))
        (goto-char (match-beginning 0)))

      ;; flash, Emacs v25 only API
      (xref-pulse-momentarily)))))


(defun counsel-ctags-remember (cand)
  "Remember CAND."
  (when counsel-ctags-debug
    (message "counsel-ctags-remember called"))
  (cl-delete-if (lambda (s) (string= (car cand) (car s))) counsel-ctags-tag-history)
  (push cand counsel-ctags-tag-history))

(defun counsel-ctags--time-cost (start-time)
  "Show time cost since START-TIME."
  (let* ((time-passed (float-time (time-since start-time))))
    (format "%.01f second%s"
            time-passed
            (if (<= time-passed 2) "" "s"))))

(defun counsel-ctags-open-tag-cand (tagname cands time)
  "Find TAGNAME from CANDS.  Open tags file at TIME."
  ;; mark current point for `pop-tag-mark'
  (let* ((dir (counsel-ctags-tags-file-directory))
         selected)
    (cond
     ;; ((= 1 (length cands))
     ;;  ;; open the file directly
     ;;  (counsel-ctags-remember (car cands))
     ;;  (counsel-ctags-open-file-api (car cands)
     ;;                               dir
     ;;                               tagname))
     ((setq selected (completing-read (format  "Find Tag (%s): "
                                               (counsel-ctags--time-cost time))
                                      cands))
      (setq selected (assoc selected cands))
      (message "selected=%s file=%s" selected (counsel-ctags-get :file selected))
      (counsel-ctags-remember selected)
      (counsel-ctags-open-file-api selected dir tagname)))))

(defun counsel-ctags-tags-file-must-exist ()
  "Make sure tags file does exist."
  (let* ((tags-file (counsel-ctags-locate-tags-file))
         src-dir)
    (when (and (not tags-file)
               ;; No need to hint after user set `counsel-ctags-extra-tags-files'
               (not counsel-ctags-extra-tags-files)
               (not counsel-ctags-can-skip-project-root))
      (setq src-dir (read-directory-name "Ctags will scan code at: "
                                         (counsel-ctags-locate-project)))
      (cond
       (src-dir
        (counsel-ctags-scan-dir src-dir)
        (setq tags-file (counsel-ctags-get-tags-file-path src-dir)))
       (t
        (error "Can't find TAGS.  Please run `counsel-ctags-scan-code'!"))))
    ;; the tags file IS touched
    (when tags-file
      (counsel-ctags-add-tags-file-to-history tags-file))))

;;;###autoload
(defun counsel-ctags-find-tag-name-default ()
  "Find tag at point."
  (let ((tag-name (find-tag-default)))
    (when (and (memq major-mode
                     counsel-ctags-major-modes-to-strip-default-tag-name)
           (string-match "^\\(`.*`\\|=.*=\\|~.*~\\|\".*\"\\|'.*'\\)$" tag-name))
      (setq tag-name (substring tag-name 1 (1- (length tag-name)))))
    tag-name))

;;;###autoload
(defun counsel-ctags-word-at-point (predicate)
  "Get word at point.  PREDICATE should return t on testing word character.

For example, get a word when dot character is part of word,

   (counsel-ctags-word-at-point (lambda (c)
                                  (or (= c ?.)
                                      (and (>= c ?0) (<= c ?9))
                                      (and (>= c ?A) (<= c ?Z))
                                      (and (>= c ?a) (<= c ?z)))))"
  (let* ((rlt (char-to-string (following-char)))
         (b (line-beginning-position))
         (e (line-end-position)))
    ;; backward
    (save-excursion
      (backward-char)
      (while (and (>= (point) b) (funcall predicate (following-char)))
        (setq rlt (concat (char-to-string (following-char)) rlt))
        (backward-char)))

    (save-excursion
      (forward-char)
      (while (and (< (point) e) (funcall predicate (following-char)))
        (setq rlt (concat rlt (char-to-string (following-char)) ))
        (forward-char)))
    rlt))

;;;###autoload
(defun counsel-ctags-scan-code (&optional dir)
  "Use Ctags to scan code at DIR."
  (interactive)
  (let* ((src-dir (or dir
                      (read-directory-name "Ctags will scan code at: "
                                           (or (counsel-ctags-locate-project)
                                               default-directory)))))
    (when src-dir
      (counsel-ctags-scan-dir src-dir))))

(defun counsel-ctags-positive-regex (patterns)
  "Extract positive regex from PATTERNS."
  (let* ((re (car patterns)))
    (cond
     ((or (not re) (string= re ""))
      "[^ \t]+")
     (t
      (ivy--regex re)))))

(defun counsel-ctags-exclusion-regex (patterns)
  "Extract exclusion PATTERNS."
  (let* ((re (cadr patterns)))
    (unless re (setq re ""))
    ;; remove trailing spaces
    (setq re (replace-regexp-in-string " +$" "" re))
    (cond
     ((string= re "")
      (setq re nil))
     (t
      (mapconcat 'ivy--regex
                 (split-string re " +")
                 "\\\|")))))

(defun counsel-ctags-list-tag-function (string current-file)
  "Find matching tags by search STRING.
Tags might be sorted by comparing tag's path with CURRENT-FILE."
  (cond
   ((< (length string) 3)
    ;; new version
    (ivy-more-chars))
   (t
    ;; I prefer build the regex by myself
    (let* ((patterns (split-string string " *!"))
           (pos-re (counsel-ctags-positive-regex patterns))
           (neg-re (counsel-ctags-exclusion-regex patterns))
           rlt)
      ;; use positive pattern to get collection
      ;; when using dynamic collection
      (setq rlt (counsel-ctags-collect-cands pos-re t current-file))
      ;; then use exclusion patterns to exclude candidates
      (when (and rlt neg-re)
        (setq rlt (delq nil (mapcar
                             `(lambda (s)
                               (unless (string-match-p ,neg-re s) s))
                             rlt))))
      (setq counsel-ctags-find-tag-candidates rlt)
      rlt))))

(defun counsel-ctags-find-tag-api (tagname fuzzy current-file)
  "Find TAGNAME using FUZZY algorithm from CURRENT-FILE."
  (let* ((time (current-time))
         (dir (counsel-ctags-tags-file-directory))
         (current-file (and current-file (file-local-name current-file))))
    (if dir (setq dir (file-local-name dir)))
    (when counsel-ctags-debug
      (message "counsel-ctags-find-tag-api called => tagname=%s fuzzy=%s dir%s current-file=%s"
               tagname
               fuzzy
               dir
               current-file))
    ;; Dir could be nil. User could use `counsel-ctags-extra-tags-files' instead
    (cond
     ((and (not dir) (not counsel-ctags-extra-tags-files))
      (message "Tags file is not ready yet."))
     ((not tagname)
      (ivy-read "Fuzz matching tags: "
                `(lambda (s)
                   (counsel-ctags-list-tag-function s ,current-file))
                :history 'counsel-git-grep-history
                :dynamic-collection t
                :action `(lambda (e)
                           (counsel-ctags-open-file-api e ,dir))))

     ((not (setq counsel-ctags-find-tag-candidates
                 (counsel-ctags-collect-cands tagname fuzzy current-file dir)))
      ;; OK, let's try grep the whole project if no tag is found yet
      (funcall counsel-ctags-fallback-grep-function
               tagname
               "No tag is found. "
               (counsel-ctags-locate-project)))

     (t
      ;; open the one selected candidate
      (counsel-ctags-open-tag-cand tagname counsel-ctags-find-tag-candidates time)))))

(defun counsel-ctags-imenu-scan-string (output)
  "Extract imenu items from OUTPUT."
  (let* (cands
         (lines (split-string output "\n")))
    (dolist (l lines)
      (let* ((items (split-string l " +"))
             (tag-name (nth 0 items))
             (tag-type (nth 1 items))
             (tag-line-num (nth 2 items)))
        (when (and (>= (length items) 4)
                   ;; tag name is not excluded
                   (not (member tag-name counsel-ctags-imenu-excluded-names))

                   ;; tags type is not excluded
                   (not (member tag-type counsel-ctags-imenu-excluded-types))
                   (string-match "[0-9]+" tag-line-num))
          (push (cons tag-name tag-line-num) cands))))
    cands))


;;;###autoload
(defun counsel-ctags-list-tag ()
  "List all tags.  Tag is fuzzy and case insensitively matched."
  (interactive)
  (counsel-ctags-tags-file-must-exist)
  (counsel-ctags-find-tag-api nil t buffer-file-name))

;;;###autoload
(defun counsel-ctags-imenu-default-create-index-function ()
  "Create an index alist for the definitions in the current buffer."
  (let* ((ctags-program (or counsel-ctags-ctags-program
                            (counsel-ctags-guess-program "ctags")))
         (ext (if buffer-file-name (file-name-extension buffer-file-name) ""))
         ;; ctags needs file extension
         (code-file (make-temp-file "coet" nil (concat "." ext)))
         cmd
         imenu-items
         cands)

    (when (and code-file (file-exists-p code-file))
      ;; write current buffer into code file
      (write-region (point-min) (point-max) code-file)
      (setq cmd
            (cond
             (counsel-ctags-command-to-scan-single-code-file
              (concat counsel-ctags-command-to-scan-single-code-file
                      "\""
                      code-file
                      "\""))
             (t
              (counsel-ctags-get-scan-command ctags-program code-file))))

      ;; create one item for imenu list
      ;; (cons name (if imenu-use-markers (point-marker) (point)))
      (setq cands (counsel-ctags-imenu-scan-string (shell-command-to-string cmd)))

      ;; cands contains list of name and line number
      ;; Example of cands:
      ;;  (setq cands (list (cons "hello" "5")))
      ;; we need convert it into imenu items (name . marker)
      (save-excursion
        (dolist (c cands)
          (let* ((name (car c)))
            (goto-char (point-min))
            (counsel-ctags-forward-line (cdr c))
            (when (search-forward name (line-end-position) t)
              (forward-char (- (length name))))
            (push (cons name (point-marker)) imenu-items))))

      ;; clean up tmp file
      (unless counsel-ctags-debug (delete-file code-file)))

    imenu-items))

;;;###autoload
(defun counsel-ctags-list-tag-in-current-file()
  "List tags in current file."
  (interactive)
  (let* ((imenu-items (counsel-ctags-imenu-default-create-index-function))
         (selected (and imenu-items
                             (completing-read "Tag names in current file: "
                                            imenu-items))))
      (when selected
        (goto-char (cdr (assoc selected imenu-items))))))

;;;###autoload
(defun counsel-ctags-find-tag ()
  "Find tag in two step.
Step 1, user need input regex to fuzzy and case insensitively match tag.
Any tag whose sub-string matches regex will be listed.

Step 2, user keeps filtering tags."
  (interactive)
  (counsel-ctags-tags-file-must-exist)
  (let* ((tagname (read-string "Regex to match tag: "
                               (or (counsel-ctags-selected-str) ""))))
    (when (and tagname (not (string= tagname "")))
        (counsel-ctags-find-tag-api tagname t buffer-file-name))))

;;;###autoload
(defun counsel-ctags-find-tag-at-point ()
  "Find tag using tagname at point.  Use `pop-tag-mark' to jump back.
Please note parsing tags file containing line with 2K characters could be slow.
That's the known issue of Emacs Lisp.  The program itself is perfectly fine."
  (interactive)
  (counsel-ctags-tags-file-must-exist)
  (let* ((tagname (counsel-ctags-tagname-at-point)))
    (cond
     (tagname
      (counsel-ctags-find-tag-api tagname nil buffer-file-name))
     (t
      (message "No tag at point")))))

;;;###autoload
(defun counsel-ctags-recent-tag ()
  "Find tag using tagname from `counsel-ctags-tag-history'."
  (interactive)
  (cond
   ((not counsel-ctags-tag-history)
    (message "`counsel-ctags-tag-history' is empty."))
   (t
    (let* ((dir (counsel-ctags-tags-file-directory))
           ;; filter the recent tags from this project
           (collection (delq nil (mapcar
                                  (lambda (e) (if (string= dir (cdr e)) e))
                           counsel-ctags-tag-history))))
      (message "collection=%s" collection)
      (when collection
        (ivy-read "Recent tag names: "
                  collection
                  :action `(lambda (e)
                             (counsel-ctags-open-file-api (car e) (cdr e)))))))))

;;;###autoload
(defun counsel-ctags-virtual-update-tags()
  "Scan code and create tags file again.
It's the interface used by other hooks or commands.
The tags updating might not happen."
  (interactive)
  (let* ((dir (and buffer-file-name
                   (file-name-directory buffer-file-name)))
         (tags-file (and counsel-ctags-tags-file-history
                         (car counsel-ctags-tags-file-history))))

    (when counsel-ctags-debug
      (message "counsel-ctags-virtual-update-tags called. dir=%s tags-file=%s" dir tags-file))

    (when (and dir
               tags-file
               (string-match-p (file-name-directory (expand-file-name tags-file))
                               (expand-file-name dir)))
      (cond
       ((or (not counsel-ctags-timer)
            (> (- (float-time (current-time)) (float-time counsel-ctags-timer))
               counsel-ctags-update-interval))

        ;; start timer if not started yet
        (setq counsel-ctags-timer (current-time))

        ;; start updating
        (if counsel-ctags-debug (message "counsel-ctags-virtual-update-tags actually happened."))

        (let* ((dir (file-name-directory (expand-file-name (counsel-ctags-locate-tags-file)))))
          (if counsel-ctags-debug (message "update tags in %s" dir))
          (funcall counsel-ctags-update-tags-backend dir)))

       (t
        ;; do nothing, can't run ctags too often
        (if counsel-ctags-debug (message "counsel-ctags-virtual-update-tags is actually skipped.")))))))

(defun counsel-ctags-unquote-regex-parens (str)
  "Unquote regexp parentheses in STR."
  (replace-regexp-in-string "\\\\[(){}]\\|[()]"
                            (lambda (s)
                              (or (cdr (assoc s '(("\\(" . "(")
                                                  ("\\)" . ")")
                                                  ("(" . "\\(")
                                                  (")" . "\\)")
                                                  ("\\{" . "{")
                                                  ("\\}" . "}"))))
                                  (error "Unexpected parenthesis: %S" s)))
                            str t t))

(defun counsel-ctags-read-keyword (hint &optional symbol-at-point)
  "Read keyword with HINT.
If SYMBOL-AT-POINT is nil, don't read symbol at point."
  (let* ((str (cond
               ((region-active-p)
                (push (counsel-ctags-selected-str) counsel-git-grep-history)
                (counsel-ctags-selected-str))
               (t
                (read-from-minibuffer hint
                                      (if symbol-at-point (thing-at-point 'symbol))
                                      nil
                                      nil
                                      'counsel-git-grep-history)))))
    (when str
      (cond
       ((region-active-p)
        (push str minibuffer-history)
        (setq counsel-ctags-keyword (counsel-ctags-unquote-regex-parens str))
        ;; de-select region
        (set-mark-command nil))
       (t
        ;; processing double quotes character
        (setq counsel-ctags-keyword (replace-regexp-in-string "\"" "\\\\\""str))))))
  counsel-ctags-keyword)

(defun counsel-ctags-has-quick-grep-p ()
  "Test if ripgrep program exist."
  (or counsel-ctags-use-ripgrep-force (counsel-ctags-guess-program "rg")))

(defun counsel-ctags-shell-quote (argument)
  "Quote ARGUMENT."
  (if (eq system-type 'windows-nt) argument
    (shell-quote-argument argument)))

(defun counsel-ctags-exclude-opts (use-cache)
  "Grep CLI options.  IF USE-CACHE is t, the options is read from cache."
  (let* ((ignore-dirs (if use-cache (plist-get counsel-ctags-opts-cache :ignore-dirs)
                        counsel-ctags-ignore-directories))
         (ignore-file-names (if use-cache (plist-get counsel-ctags-opts-cache :ignore-file-names)
                              counsel-ctags-ignore-filenames)))
    ;; please note Windows DOS CLI only support double quotes
    (cond
     ((counsel-ctags-has-quick-grep-p)
      (concat (mapconcat (lambda (e)
                           (format "-g=\"!%s/*\"" (counsel-ctags-shell-quote e)))
                         ignore-dirs " ")
              " "
              (mapconcat (lambda (e)
                           (format "-g=\"!%s\"" (counsel-ctags-shell-quote e)))
                         ignore-file-names " ")))
     (t
      (concat (mapconcat (lambda (e)
                           (format "--exclude-dir=\"%s\"" (counsel-ctags-shell-quote e)))
                         ignore-dirs " ")
              " "
              (mapconcat (lambda (e)
                           (format "--exclude=\"%s\"" (counsel-ctags-shell-quote e)))
                         ignore-file-names " "))))))

(defun counsel-ctags-grep-cli (keyword &optional use-cache)
  "Use KEYWORD and USE-CACHE to build CLI.
Extended regex is used, like (pattern1|pattern2)."
  (cond
   ((counsel-ctags-has-quick-grep-p)
    ;; "--hidden" force ripgrep to search hidden files/directories, that's default
    ;; behavior of grep
    (format "\"%s\" %s %s --hidden %s \"%s\" --"
            ;; if rg is not in $PATH, then it's in `counsel-ctags-grep-program'
            (or (counsel-ctags-guess-program "rg") counsel-ctags-grep-program)
            ;; (if counsel-ctags-debug " --debug")
            counsel-ctags-ripgrep-default-options
            counsel-ctags-grep-extra-arguments
            (counsel-ctags-exclude-opts use-cache)
            keyword))
   (t
    ;; use extended regex always
    (format "\"%s\" -rsnE %s %s \"%s\" *"
            (or counsel-ctags-grep-program (counsel-ctags-guess-program "grep"))
            counsel-ctags-grep-extra-arguments
            (counsel-ctags-exclude-opts use-cache)
            keyword))))

(defun counsel-ctags-parent-directory (level directory)
  "Return LEVEL up parent directory of DIRECTORY."
  (let* ((rlt directory))
    (while (and (> level 0) (not (string= "" rlt)))
      (setq rlt (file-name-directory (directory-file-name rlt)))
      (setq level (1- level)))
    (if (string= "" rlt) (setq rlt nil))
    rlt))

(defun counsel-ctags-dirname (directory)
  "Get DIRECTORY name without parent."
  (file-name-as-directory (file-name-base (directory-file-name directory))))

;;;###autoload
(defun counsel-ctags-grep (&optional default-keyword hint root show-keyword-p)
  "Grep at project with best grep program (ripgrep, grep...) automatically.
Extended regex like (pattern1|pattern2) is used.
If DEFAULT-KEYWORD is not nil, it's used as grep keyword.
If HINT is not nil, it's used as grep hint.
ROOT is the directory to grep.  It's automatically detected.
If current file is org file, current node or parent node's property
\"GREP_PROJECT_ROOT\" is read to get the root directory to grep.
If SHOW-KEYWORD-P is t, show the keyword in the minibuffer.

This command uses Ivy which supports regexp negation with \"!\".
For example, \"define key ! ivy quit\" first selects everything
matching \"define.*key\", then removes everything matching \"ivy\",
and finally removes everything matching \"quit\". What remains is the
final result set of the negation regexp."
  (interactive)

  (unless hint
    (setq hint (if (eq counsel-ctags-convert-grep-keyword 'identity)
                   "Regular expression for grep: "
                 "Keyword for searching: ")))

  (let* ((text (if default-keyword default-keyword
                  (counsel-ctags-read-keyword hint)))
         (keyword (funcall counsel-ctags-convert-grep-keyword text))
         (default-directory (expand-file-name (or root
                                                  (counsel-ctags-org-entry-get-project-root)
                                                  (counsel-ctags-locate-project)
                                                  default-directory)))
         (time (current-time))
         (cmd (counsel-ctags-grep-cli keyword nil))
         (cands (split-string (shell-command-to-string cmd) "[\r\n]+" t))
         (dir-summary (counsel-ctags-dirname default-directory)))

    (when (and cands
               buffer-file-name
               counsel-ctags-sort-grep-result-p
               counsel-ctags-candidates-sorted-p)
      ;; grep should not waste time on lisp version of string distance
      ;; So `string-distance' from Emacs 27 is required
      (let* ((ref (file-relative-name buffer-file-name root)))
        (setq cands
              (sort cands
                    `(lambda (a b)
                       (< (string-distance (car (split-string a ":")) ,ref t)
                          (string-distance (car (split-string b ":")) ,ref t)))))))

    (when counsel-ctags-debug
      (message "counsel-ctags-grep called: keyword=%s\n  root=%s\n  cmd=%s\n  cands=%s"
               keyword default-directory cmd cands))
    (counsel-ctags-put :ignore-dirs
                       counsel-ctags-ignore-directories
                       counsel-ctags-opts-cache)

    (counsel-ctags-put :ignore-file-names
                       counsel-ctags-ignore-filenames
                       counsel-ctags-opts-cache)

    ;; Slow down grep 10 times
    (ivy-read (concat hint (format "Grep \"%s\" at %s (%s): "
                                   text
                                   dir-summary
                                   (counsel-ctags--time-cost time)))
              cands
              :history 'counsel-git-grep-history ; share history with counsel
              :action `(lambda (item)
                         ;; when grepping, we grepping in project root
                         (counsel-ctags-open-file-api item
                                                      ,default-directory
                                                      ,keyword))
              :initial-input (if show-keyword-p keyword)
              :caller 'counsel-ctags-grep)))

;;;###autoload
(defun counsel-ctags-grep-current-directory (&optional level)
  "Grep current directory or LEVEL up parent directory."
  (interactive "P")
  (unless level (setq level 0))
  (let* ((root (counsel-ctags-parent-directory level default-directory)))
    (counsel-ctags-grep nil nil root)))

;;;###autoload
(defun counsel-ctags-update-tags-force (&optional forced-tags-file)
  "Update current tags file using default implementation.
If FORCED-TAGS-FILE is nil, the updating process might now happen."
  (interactive)
  (let* ((tags-file (or forced-tags-file
                        (counsel-ctags-locate-tags-file))))
    (when tags-file
      ;; @see https://github.com/redguardtoo/counsel-ctags/issues/82
      ;; If code file is moved and TAGS is updated, invalidate the cache.
      (counsel-ctags-cache-invalidate tags-file)
      ;; scan the code now
      (counsel-ctags-scan-dir (file-name-directory (expand-file-name tags-file)))
      (unless counsel-ctags-quiet-when-updating-tags
        (message "%s is updated!" tags-file)))))

;;;###autoload
(defun counsel-ctags-tag-line (tag-name file code-snippet )
  "One line in tag file using TAG-NAME, FILE, CODE-SNIPPET."
  ;; todo, change to vim ctags format
  (format "%s\t%s\t/^%s$/;\"\tf\n"
          tag-name
          file
          code-snippet))

;;;###autoload
(defun counsel-ctags-append-to-tags-file (sections tags-file)
  "Append SECTIONS into TAGS-FILE.
Each section is a pair of file and tags content in that file.
File can be url template like \"https//myproj.com/docs/API/%s\".
The `counsel-ctags-browse-url-function' is used to open the url."
  (when (and tags-file
             (file-exists-p tags-file)
             (file-readable-p tags-file)
             (file-writable-p tags-file)
             sections
             (> (length sections) 0))

    (with-temp-buffer
      (insert-file-contents tags-file)
      (goto-char (point-max))
      (dolist (s sections)
        (when (and (car s) (cdr s))
          (insert (format "\n\014\n%s,%d\n%s" (car s) 0 (cdr s)))))
      (write-region (point-min) (point-max) tags-file nil :silent))))

;; {{ occur setup
(defun counsel-ctags-grep-occur (&optional _cands)
  "Open occur buffer for `counsel-ctags-grep'."
  (unless (eq major-mode 'ivy-occur-grep-mode)
    (ivy-occur-grep-mode)
    (font-lock-mode -1))
  ;; useless to set `default-directory', it's already correct
  ;; we use regex in elisp, don't unquote regex
  (let* ((cmd (counsel-ctags-grep-cli counsel-ctags-keyword t))
         (cands (ivy--filter ivy-text
                             (split-string (shell-command-to-string cmd)
                                           "[\r\n]+" t))))
    (when counsel-ctags-debug
      (message "counsel-ctags-grep-occur called. cmd=%s" cmd))
    (swiper--occur-insert-lines
     (mapcar
      (lambda (cand) (concat "./" cand))
      cands))))

(ivy-set-occur 'counsel-ctags-grep 'counsel-ctags-grep-occur)
;; }}

(provide 'counsel-ctags)
;;; counsel-ctags.el ends here
