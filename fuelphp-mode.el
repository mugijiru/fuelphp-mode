;(add-to-list 'load-path (expand-file-name "~/Projects/git/hoge/fuelphp-mode"))
;(require 'fuelphp-mode)

(require 'cl)
(defvar fuelphp-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c ; f c") 'fuelphp-find-controller)
    (define-key map (kbd "C-c ; f m") 'fuelphp-find-model)
    map)
  "Keymap for `fuelphp-mode'.")



(defun fuelphp-recursive-directory-files (dir)
  "再帰的lsみたいなの"
  (message dir)
  (let* ((files (directory-files dir))
         (files (loop for file in files
                      do (message file)
                      if (not (string-match "/?\\.\\{1,2\\}$" file))
                      collect file)))
    (fuelphp-flatten
     (loop for file in files
           collect (if (file-directory-p (concat dir "/" file))
                       (loop for path in (fuelphp-recursive-directory-files (concat dir "/" file))
                             collect (concat file "/" path))
                     file)))))

(defun fuelphp-flatten (list)
  (cond
   ((null list) nil)
   ((atom list) (list list))
   (t
    (append (fuelphp-flatten (car list)) (fuelphp-flatten (cdr list))))))


(defun fuelphp-find-controller ()
  "今いるディレクトリが
fuel/app/views
fuel/app/classes/model
fuel/app/classes/controller
辺りの時にcontrollerをfindするメソッド。
とりあえずは一発での移動機能は無し
"
  (interactive)
  (let ((dir default-directory)
        (controller-path "/fuel/app/classes/controller/"))
    (if (string-match "^\\(.*\\)/fuel/app/\\(classes/model\\|classes/controller\\|views\\)/" dir)
        (find-file
         (concat (match-string 1 dir) controller-path
                 (ido-completing-read
                  "Controller: "
                  (let ((dir-names (fuelphp-recursive-directory-files (concat (match-string 1 dir) controller-path))))
                    (loop for dir-name in dir-names
                          if (string-match "\\.php$" dir-name)
                          collect dir-name))))))))

(defun fuelphp-find-model ()
  "今いるディレクトリが
fuel/app/views
fuel/app/classes/model
fuel/app/classes/controller
辺りの時にcontrollerをfindするメソッド。
とりあえずは一発での移動機能は無し
"
  (interactive)
  (let ((dir default-directory)
        (model-path "/fuel/app/classes/model/"))
    (if (string-match "^\\(.*\\)/fuel/app/\\(classes/model\\|classes/controller\\|views\\)/" dir)
        (find-file
         (concat (match-string 1 dir) model-path
                 (ido-completing-read
                  "Controller: "
                  (let ((dir-names (fuelphp-recursive-directory-files (concat (match-string 1 dir) model-path))))
                    (loop for dir-name in dir-names
                          if (string-match "\\.php$" dir-name)
                          collect dir-name))))))))

(defun fuelphp-root (&optional dir)
  (or dir (setq dir default-directory))
  (message dir)
  (if (file-exists-p (expand-file-name
                      "config.php" (expand-file-name "fuel/app/config" dir)))
      dir
    (let ((new-dir (expand-file-name (file-name-as-directory "..") dir)))
      ;; regexp to match windows roots, tramp roots, or regular posix roots
      (unless (string-match "\\(^[[:alpha:]]:/$\\|^/[^\/]+:/?$\\|^/$\\)" dir)
        (fuelphp-root new-dir)))))

(defun fuelphp-launch ()
  (let ((root (fuelphp-root)))
    (if root
        (fuelphp-mode 1)
      (fuelphp-mode -1))))


(defadvice cd (after fuelphp-on-cd activate)
  (fuelphp-launch))

(add-hook 'find-file-hook 'fuelphp-launch)

(define-minor-mode fuelphp-mode
  "fuelphp mode for emacs"
  nil
  :keymap fuelphp-mode-map
  :lighter " fuelphp")

(provide 'fuelphp-mode)
