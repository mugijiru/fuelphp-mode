;;; fuelphp-mode.el --- Minor mode for FuelPHP project

;; Copyright (C) 2013 by Mugijiru

;; Author: Mugijiru <mugijiru.dev@gmail.com>
;; URL: https://github.com/mugijiru/fuelphp-mode
;; Version: 0.01
;; Package-Requires: cl

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

;;; Code:


(require 'cl)

(defgroup fuelphp nil
  "fuelphp-mode カスタマイズ"
  :prefix "fuelphp-"
  :group 'fuelphp)

(defcustom fuelphp-tags-file-name
  "TAGS"
  "FuelPHPプロジェクトのTAGファイルのPATH"
  :group 'fuelphp)

(defvar fuelphp-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c . f c") 'fuelphp-find-controller)
    (define-key map (kbd "C-c . f m") 'fuelphp-find-model)
    (define-key map (kbd "C-c . f v") 'fuelphp-find-view)
    (define-key map (kbd "C-c . o s") 'fuelphp-server)
    (define-key map (kbd "C-c . o c") 'fuelphp-console)
    (define-key map (kbd "C-c . o g") 'fuelphp-generate)
    (define-key map (kbd "C-c . V") 'fuelphp-version)
    (define-key map (kbd "C-c . g") 'fuelphp-rgrep)
    map)
  "Keymap for `fuelphp-mode'.")

(setq fuelphp-file-type-alist
      '(("m" "model")
        ("c" "controller")
        ("v" "views")))

(defun fuelphp-find-file ()
  "find model とかを1まとめにしたい"
  (interactive)
  (let* ((type (cadr (assoc (completing-read "" fuelphp-file-type-alist nil t) fuelphp-file-type-alist)))
         (root (fuelphp-root))
         (path (concat "/fuel/app/classes/" (file-name-nondirectory type) "/")))
    (if root
        (find-file
         (concat root "/" path
                 (ido-completing-read
                  (concat (capitalize type) ": ")
                  (let ((dir-names (fuelphp-recursive-directory-files (concat root path))))
                    (loop for dir-name in dir-names
                          if (string-match "\\.php$" dir-name)
                          collect dir-name))))))))

(defun fuelphp-recursive-directory-files (dir)
  "再帰的lsみたいなの"
  (let* ((files (directory-files dir))
         (files (loop for file in files
                      if (not (string-match "/?\\.\\{1,2\\}$" file))
                      collect file)))
    (fuelphp-flatten
     (loop for file in files
           collect (if (file-directory-p (concat dir "/" file))
                       (loop for path in (fuelphp-recursive-directory-files (concat dir "/" file))
                             collect (concat file "/" path))
                     file)))))

(defun fuelphp-flatten (list)
  "入れ子になったリストをflatにする関数
flatten nested alist."
  (let ((ret-list '()))
    (loop for item in list
          if (consp item)
          do (loop for i in (fuelphp-flatten item)
                   do (push i ret-list))
          if (atom item)
          do (push item ret-list))
    (reverse ret-list)))

(defun fuelphp-server ()
  "Start FuelPHP webserver."
  (interactive)
  (let ((command "server")
        (dir (fuelphp-root)))
    (if dir
        (if (<= 1.6 (fuelphp-version))
            (fuelphp-oil-execute command)
          (message "'fuelphp-server' command could not execute version 1.5 or earlier")))))

(defun fuelphp-console ()
  "Start FuelPHP console."
  (interactive)
  (let ((command "console")
        (dir (fuelphp-root)))
    (if dir
        (fuelphp-oil-execute command))))

(defun fuelphp-generate ()
  "execute oil generate"
  (interactive)
  (let* ((types '("controller" "model" "migration" "scaffold" "views"))
         (type (completing-read "ganerate type: "
                         types
                         nil
                         t))
         (options (split-string (read-string "options: "))))
     (with-current-buffer "*oil generate*"
       (erase-buffer))
     (pop-to-buffer
      (save-excursion
        (cd (fuelphp-root))
        (apply 'make-comint "oil generate" "php" nil "oil" "g" type options)))))

(defun fuelphp-rgrep ()
  (interactive)
  (grep-compute-defaults)
  (let ((regex))
    (funcall 'rgrep (read-from-minibuffer "search for: " regex)
             "*.*"
             (concat (fuelphp-root) "/fuel/app"))))

(defun fuelphp-oil-execute (command)
  "Execute oil command."
  (pop-to-buffer
   (save-excursion
       (cd (fuelphp-root))
       (make-comint "oil server" "php" nil "oil" command))))

(defun fuelphp-oil-execute-and-get-result (command)
  "Execute oil command and return command output."
  (let ((dir (fuelphp-root)))
    (with-current-buffer (get-buffer-create "*oil*")
      (save-excursion
        (erase-buffer)
        (cd dir)
        (call-process "php" nil "*oil*" nil "oil" command)
        (buffer-string)))))

;; call-process program &optional infile buffer-name display &rest args

(defun fuelphp-find-model ()
  "今いるディレクトリが
fuelphpのプロジェクト内の時に
辺りの時にmodelをfindするメソッド。
とりあえずは一発での移動機能は無し

Find Model files in FuelPHP's project
"
  (interactive)
  (let ((root (fuelphp-root))
        (path "/fuel/app/classes/model/"))
    (if root
        (find-file
         (concat root "/" path
                 (ido-completing-read
                  "Model: "
                  (let ((dir-names (fuelphp-recursive-directory-files (concat root path))))
                    (loop for dir-name in dir-names
                          if (string-match "\\.php$" dir-name)
                          collect dir-name))))))))

(defun fuelphp-find-controller ()
  "今いるディレクトリが
fuelphpのプロジェクト内の時に
辺りの時にcontrollerをfindするメソッド。
とりあえずは一発での移動機能は無し

Find Controller files in FuelPHP's project
"
  (interactive)
  (let ((root (fuelphp-root))
        (path "/fuel/app/classes/controller/"))
    (if root
        (find-file
         (concat root "/" path
                 (ido-completing-read
                  "Controller: "
                  (let ((dir-names (fuelphp-recursive-directory-files (concat root path))))
                    (loop for dir-name in dir-names
                          if (string-match "\\.php$" dir-name)
                          collect dir-name))))))))

(defun fuelphp-find-view ()
  "今いるディレクトリが
fuelphpのプロジェクト内の時に
辺りの時にviewをfindするメソッド。
とりあえずは一発での移動機能は無し

Find View files in FuelPHP's project
"
  (interactive)
  (let ((root (fuelphp-root))
        (path "/fuel/app/views/"))
    (if root
        (find-file
         (concat root "/" path
                 (ido-completing-read
                  "View: "
                  (let ((dir-names (fuelphp-recursive-directory-files (concat root path))))
                    (loop for dir-name in dir-names
                          if (string-match "\\.php$" dir-name)
                          collect dir-name))))))))


(defun fuelphp-root (&optional dir)
  "Find FuelPHP Project root directory.
If not FuelPHP directory, then return nil."
  (let* ((dir (or dir default-directory))
         (config-dir (expand-file-name "fuel/app/config" dir)))
    (if (file-exists-p (expand-file-name "config.php" config-dir))
        dir
      (let ((parent-dir (expand-file-name (file-name-as-directory "..") dir)))
        (unless (string= "/" dir)
          (fuelphp-root parent-dir))))))

(defun fuelphp-launch ()
"launch fuelphp-mode"
  (let* ((root (fuelphp-root))
        (tag-file-path (concat root "/" fuelphp-tags-file-name)))
    (if root
        (progn
          (set (make-local-variable 'tags-file-name)
               (and (file-exists-p tag-file-path) tag-file-path))
          (add-hook 'after-save-hook 'fuelphp-create-tags nil t)
          (fuelphp-mode 1))
      (fuelphp-mode -1))))

(defun fuelphp-create-tags ()
  "create etags for the fuelphp project"
  (interactive)
  (let ((root (fuelphp-root)))
    (save-excursion
      (with-temp-buffer
        (switch-to-buffer (current-buffer))
        (cd root)
        (shell-command
         (format "find %s -type f -name '*.php' | etags -R -f %s -"
                 root
                 fuelphp-tags-file-name)
         nil nil)))))

(defun fuelphp-version ()
  "Get fuelphp version."
  (string-to-number (fuelphp-version-str)))

(defun fuelphp-version-str ()
  "Get fuelphp version."
  (substring (fuelphp-version-full) 6 9))

(defun fuelphp-version-full ()
  "Get fuelphp version full information."
  (let ((dir (fuelphp-root))
        (command "--version"))
    (if dir
        (let ((version-str (fuelphp-oil-execute-and-get-result command)))
          (replace-regexp-in-string "[\n\r]+$" "" version-str)))))

(defun fuelphp-version-minibuffer ()
  "display fuelphp version full information."
  (interactive)
  (message (fuelphp-version-full)))

(defadvice cd (after fuelphp-on-cd activate)
  (fuelphp-launch))

(add-hook 'find-file-hook 'fuelphp-launch)
(add-hook 'window-configuration-change-hook 'fuelphp-launch)

(define-minor-mode fuelphp-mode
  "FuelPHP mode for emacs"
  nil
  :keymap fuelphp-mode-map
  :lighter " fuelphp")

(provide 'fuelphp-mode)

;;; fuelphp-mode.el ends here
