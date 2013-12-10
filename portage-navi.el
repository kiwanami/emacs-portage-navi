;;; portage-navi.el --- portage viewer

;; Copyright (C) 2013  SAKURAI Masashi

;; Author:  <m.sakurai at kiwanami.net>
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'concurrent)
(require 'ctable)
(require 'xml)

(defface pona:face-title 
  '((((class color) (background light))
     :foreground "Deeppink2" :height 1.5 :inherit variable-pitch)
    (((class color) (background dark))
     :foreground "yellow" :weight bold :height 1.5 :inherit variable-pitch)
    (t :height 1.5 :weight bold :inherit variable-pitch))
  "Face for pona titles at level 1."
  :group 'pona)

(defface pona:face-subtitle
  '((((class color) (background light))
     (:foreground "Gray10" :height 1.2 :inherit variable-pitch))
    (((class color) (background dark))
     (:foreground "Gray90" :height 1.2 :inherit variable-pitch))
    (t :height 1.2 :inherit variable-pitch))
  "Face for pona titles at level 2."
  :group 'pona)

(defface pona:face-item
  '((t :inherit variable-pitch :foreground "DarkSlateBlue"))
  "Face for pona items."
  :group 'pona)


(defvar pona:portage-dir "/usr/portage" "pona:portage-dir.")

(defvar pona:cache-category-list nil "pona:cache-category-list.")

(defun pona:category-list ()
  "category-list"
  (unless pona:cache-category-list
    (setq pona:cache-category-list (directory-files pona:portage-dir nil "^[^.]")))
  pona:cache-category-list)

(defun pona:category-package-list-d (category)
  "package-list
CATEGORY"
  (deferred:$
    (deferred:process-buffer "eix" "--xml" "-C" category)
    (deferred:nextc it
      (lambda (buf)
        (let* ((dom (with-current-buffer buf
                      (xml-parse-region)))
               (cats (xml-get-children (car dom) 'category)))
          (xml-get-children (car cats) 'package))))))

(defun pona:search-category-package-list-d (search-text)
  "search-package-list-d
SEARCH-TEXT"
  (deferred:$
    (deferred:process-buffer "eix" "--xml" "-S" search-text)
    (deferred:nextc it
      (lambda (buf)
        (let* ((dom (with-current-buffer buf
                      (xml-parse-region)))
               (cats (xml-get-children (car dom) 'category)))
          cats)))))

(defun pona:package-equery-use-d (package)
  (deferred:process "equery" "-C" "uses" (xml-get-attribute package 'name)))



(defvar pona:category-buffer-name " *pona:category-buffer")

(defface pona:face-toolbar-button
  '((((class color) (background light))
     :foreground "Lightskyblue4" :background "White")
    (((class color) (background dark))
     :foreground "Gray10" :weight bold))
  "Face for button on toolbar" :group 'pona)

(defun pona:render-button (title command)
  "[internal] Return a decorated text for the toolbar buttons.
TITLE is a button title.  COMMAND is a interactive command
function called by clicking."
  (let ((text (concat "[" title "]"))
        (keymap (make-sparse-keymap)))
    (define-key keymap [mouse-1] command)
    (define-key keymap (kbd "RET") command)
    (define-key keymap (kbd "C-m") command)
    (propertize text
                'face 'pona:face-toolbar-button
                'keymap keymap
                'mouse-face 'highlight)))

(defun pona:render-link (title command)
  "[internal] Return a decorated text for the link buttons.
TITLE is a button title.  COMMAND is a interactive command
function called by clicking."
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap [mouse-1] command)
    (define-key keymap (kbd "RET") command)
    (define-key keymap (kbd "C-m") command)
    (propertize title
                'face 'pona:face-item
                'keymap keymap
                'mouse-face 'highlight)))

(defun pona:category-buffer ()
  "category-buffer"
  (let ((buf (get-buffer-create pona:category-buffer-name)))
    (with-current-buffer buf
      (let ((buffer-read-only))
        (erase-buffer)
        (insert (propertize "Category View" 'face 'pona:face-subtitle) "\n\n")
        (insert (pona:render-button "Search" 'pona:open-search-buffer) "\n\n")
        (loop for i in (pona:category-list)
              for line = i
              do (insert (pona:render-link line 'pona:jump-to-category) "\n"))
        (hl-line-highlight))
      (goto-char (point-min))
      (setq buffer-read-only t))
    buf))

(defun pona:jump-to-category ()
  (interactive)
  (let* ((text (thing-at-point 'line))
         (catnam (replace-regexp-in-string "\n+$" "" text)))
    (pona:open-list-category-buffer catnam)))

(defun pona:open-category-buffer ()
  "open-category-buffer"
  (interactive)
  (switch-to-buffer (pona:category-buffer)))


(defun pona:package-item-gen (package item)
  (car (xml-node-children
        (car (xml-get-children package item)))))

(defun pona:package-description (package)
  (pona:package-item-gen package 'description))

(defun pona:package-licenses (package)
  (pona:package-item-gen package 'licenses))

(defun pona:package-homepage (package)
  (pona:package-item-gen package 'homepage))

(defun pona:package-latest-version (package)
  "package-latest-version
PACKAGE"
  (let* ((vers (xml-get-children package 'version))
         latest)
    (loop with latest-ver = nil
          for v in vers
          for id = (xml-get-attribute v 'id)
          do
          (when (or (null latest) (string< latest-ver id))
            (setq latest v latest-ver id)))
    latest))

(defun pona:package-installed-version (package)
  "package-installed-version
PACKAGE"
  (let* ((vers (xml-get-children package 'version))
         installed)
    (loop for v in vers 
          for id = (xml-get-attribute v 'id)
          if (xml-get-attribute-or-nil v 'installed)
          return v)))

(defvar pona:list-buffer-name " *pona:list-buffer")

(defun pona:make-package-table--line (package)
  "make-package-table--line
PACKAGE"
  (let* ((name (xml-get-attribute package 'name))
         (vers (xml-get-children package 'version))
         latest installed)
    (loop with latest-ver = nil
          for v in vers 
          for id = (xml-get-attribute v 'id)
          for installed-ver = (xml-get-attribute-or-nil v 'installed)
          do
          (when installed-ver (setq installed v))
          (when (or (null latest) (string< latest-ver id))
            (setq latest v latest-ver id)))
    (list name
          (if installed (xml-get-attribute installed 'id) "")
          (xml-get-attribute latest 'id)
          (pona:package-description package)
          package)))

(defun pona:make-package-table (buf packages)
  "make-package-table
PACKAGES"
  (let* ((param 
          (copy-ctbl:param ctbl:default-rendering-param))
         (column-models
          (list 
           (make-ctbl:cmodel :title "No" :align 'right)
           (make-ctbl:cmodel :title "Name" :align 'left)
           (make-ctbl:cmodel :title "Installed" :align 'left)
           (make-ctbl:cmodel :title "Latest" :align 'left)
           (make-ctbl:cmodel :title "Description" :align 'left)))
         (data
          (loop for i in packages
                for no from 1
                collect (cons no (pona:make-package-table--line i))))
         (model
          (make-ctbl:model
           :column-model column-models :data data))
         component)
    (setf (ctbl:param-fixed-header param) t)
    (setq component
          (ctbl:create-table-component-buffer
           :buffer buf :model model :param param))
    (ctbl:cp-add-click-hook
     component (lambda () 
                 (let* ((row (ctbl:cp-get-selected-data-row component))
                        (package (nth 5 row)))
                   (when package
                     (pona:open-package-detail-buffer package)))))
    (ctbl:cp-get-buffer component)))

(defun pona:display-message (buf message)
  "display-message
MESSAGE"
  (with-current-buffer buf
    (let ((buffer-read-only))
      (erase-buffer)
      (insert message "\n"))))

(defun pona:list-package-buffer-gen (deferred-packages)
  "list-category-buffer-gen"
  (lexical-let
      ((buf (get-buffer-create pona:list-buffer-name)))
    (pona:display-message buf "[processing...]")
    (deferred:try
      (deferred:$ deferred-packages
        (deferred:nextc it
          (lambda (packages)
            (pona:make-package-table buf packages))))
      :catch
      (lambda (err)
        (pona:display-message buf (concat "[error!]\n" (pp-to-string err)))))
    buf))

(defun pona:open-list-category-buffer (cat-name)
  "list-category-buffer
CAT-NAME"
  (pop-to-buffer
   (pona:list-package-buffer-gen
    (pona:category-package-list-d cat-name))))



(defun pona:make-category-package-table (buf categories)
  "make-category-package-table"
  (let* ((param 
          (copy-ctbl:param ctbl:default-rendering-param))
         (column-models
          (list 
           (make-ctbl:cmodel :title "No" :align 'right)
           (make-ctbl:cmodel :title "Category" :align 'left)
           (make-ctbl:cmodel :title "Name" :align 'left)
           (make-ctbl:cmodel :title "Installed" :align 'left)
           (make-ctbl:cmodel :title "Latest" :align 'left)
           (make-ctbl:cmodel :title "Description" :align 'left)))
         (data
          (loop with ret = nil
                with no = 0
                for c in categories
                for cn = (xml-get-attribute c 'name) do
                (loop for p in (xml-get-children c 'package) do
                      (push 
                       (cons 
                        (incf no)
                        (cons
                         cn (pona:make-package-table--line p))) ret))
                finally return (nreverse ret)))
         (model
          (make-ctbl:model
           :column-model column-models :data data))
         component)
    (setf (ctbl:param-fixed-header param) t)
    (setq component
          (ctbl:create-table-component-buffer
           :buffer buf :model model :param param))
    (ctbl:cp-add-click-hook
     component (lambda () 
                 (let* ((row (ctbl:cp-get-selected-data-row component))
                        (package (nth 6 row)))
                   (when package
                     (pona:open-package-detail-buffer package)))))
    (ctbl:cp-get-buffer component)))

(defun pona:list-category-package-buffer-gen (deferred-catpacks)
  "list-category-package-buffer-gen"
  (lexical-let
      ((buf (get-buffer-create pona:list-buffer-name)))
    (pona:display-message buf "[processing...]")
    (deferred:try
      (deferred:$ deferred-catpacks
        (deferred:nextc it
          (lambda (categories)
            (pona:make-category-package-table buf categories))))
      :catch
      (lambda (err)
        (pona:display-message buf (concat "[error!]\n" (pp-to-string err)))))
    buf))

(defun pona:open-search-buffer (&optional text)
  "exec-search"
  (interactive)
  (let ((search-text (or text (read-string "Search text: "))))
    (pop-to-buffer
     (pona:list-category-package-buffer-gen
      (pona:search-category-package-list-d search-text)))))



(defun pona:rt-format (text &rest args)
  "[utility] Format strings with faces. TEXT is format
string. ARGS is a list of cons cell, ([string] . [face name])."
  (apply 'format (propertize text 'face 'pona:face-item)
         (loop for i in args
               if (consp i)
               collect (propertize (car i) 'face (cdr i))
               else
               collect (propertize i 'face 'pona:face-subtitle))))

(defun pona:define-keymap (keymap-list)
  "[internal] Key map definition utility.
KEYMAP-LIST is a source list like ((key . command) ... )."
  (let ((map (make-sparse-keymap)))
    (mapc
     (lambda (i)
       (define-key map
         (if (stringp (car i))
             (read-kbd-macro (car i)) (car i))
         (cdr i)))
     keymap-list)
    map))

(defun pona:kill-this-buffer ()
  (interactive)
  (kill-this-buffer))

(defvar pona:package-detail-mode-map 
  (pona:define-keymap
   '(
     ("q" . pona:kill-this-buffer))))

(defvar pona:package-detail-mode-hook nil
  "pona:package-detail-mode-hook.")

(defun pona:package-detail-mode (&optional arg)
  "Set up major mode `pona:package-detail-mode'.

\\{pona:package-detail-mode-map}"
  (kill-all-local-variables)
  (setq truncate-lines t)
  (use-local-map pona:package-detail-mode-map)
  (setq major-mode 'pona:package-detail-mode
        mode-name "Package Detail Mode")
  (setq buffer-undo-list t)
  (run-hooks 'pona:package-detail-mode-hook))



(defvar pona:package-detail-buffer-name " *pona:package-detail-buffer")

(defun pona:open-package-detail-buffer (package)
  "open-package-detail-buffer
PACKAGE"
  (let ((buf (get-buffer-create pona:package-detail-buffer-name))
        (lver (pona:package-latest-version package))
        (iver (pona:package-installed-version package)))
    (with-current-buffer buf
      (let ((buffer-read-only))
        (erase-buffer)
        (pona:package-detail-mode)
        (insert
         (pona:rt-format
          "%s\nLatest: %s  Installed: %s\n"
          (cons (xml-get-attribute package 'name) 'pona:face-title)
          (xml-get-attribute lver 'id) 
          (or (and iver (xml-get-attribute iver 'id)) "(not installed)")))
        (insert
         (pona:rt-format
          "Description: %s\nLicenses: %s\nHomepage: %s\n"
          (cons (pona:package-description package) nil)
          (cons (pona:package-licenses package) nil)
          (cons (pona:package-homepage package) nil)))
        (insert (propertize  "Use flags:\n" 'face 'pona:face-item))
        (goto-char (point-min))
        (pona:insert-deferred
         buf (pona:package-equery-use-d package)))
      (setq buffer-read-only t))
    (pop-to-buffer buf)))

(defun pona:insert-deferred (buf d)
  (lexical-let ((buf buf))
    (deferred:nextc d
      (lambda (text) 
        (with-current-buffer buf
          (let ((buffer-read-only))
            (goto-char (point-max))
            (insert text)
            (goto-char (point-min))))))))



(defun pona:portage-navi ()
  (interactive)
  (pona:open-category-buffer))

;; (progn (eval-current-buffer) (pona:open-category-buffer))
;; (pona:open-list-category-buffer "app-text")
;; (pona:open-search-buffer "chrome")

(provide 'portage-navi)
;;; portage-navi.el ends here
