
(defconst api "http://localhost:3000"
  "API ADDRESS.")

(defconst login-url "/login/cellphone"
  "Login url pattern.")

(defconst playlist-url "/user/playlist"
  "Playlist url pattern.")

(defconst user-detail-url "/user/detail"
  "User detail url pattern.")

(defconst login-args "?phone=%s&password=%s"
  "Login args.")

(defconst user-detail-args "?uid=%s"
  "User detail args.")

(defun format-login-args (phone password)
  "Format login args."
  (format login-args phone password))

(defun format-user-detail-args (uid)
  "Format user detail args."
  (format user-detail-args uid))

(defvar user-id nil
  "User id.")

(defvar user-password nil
  "User password.")

(defvar nickname nil
  "User nickname.")

(defvar avatar-url nil
  "User avatar url.")

(defun format-request-url (url args)
  (concat api url args))

(defun netease-music ()
  (interactive)
  (switch-to-buffer "netease-music")
  (netease-music-mode)
  (netease-music-init))

(define-derived-mode netease-music-mode org-mode "netease-music")

(defun netease-music-init ()
  (setq phone (read-string "Your Phone Number Please: "))
  (setq password (read-string "Your Password: "))
  (netease-music-login phone password)
  (set-user-details user-id))

(defun netease-music-login (username password)
  (let* ((json (request login-url (format-login-args phone password))))
    (setq user-id (set-user-id json))))

(defun set-user-id (json)
  "Return user id from JSON."
  (cdr (assoc 'id (cdr (assoc 'account json)))))

(defun set-user-nickname (json)
  (cdr (assoc 'nickname (cdr (assoc 'profile json)))))

(defun set-user-avatar-url (json)
  (cdr (assoc 'avatarUrl (cdr (assoc 'profile json)))))

(defun set-user-details (user-id)
  (let* ((json (request user-detail-url (format-user-detail-args user-id))))
    (setq nickname (set-user-nickname))
    (setq avatar-url (set-user-avatar-url))))

(defun request (url-pattern args)
  (let (json)
    (with-current-buffer (url-retrieve-synchronously
                          (format-request-url url-pattern args))
      (set-buffer-multibyte t)
      (goto-char (point-min))
      (re-search-forward "^$" nil 'move)
      (setq json (json-read-from-string
                  (buffer-substring-no-properties (point) (point-max))))
      (kill-buffer (current-buffer)))
    json))
