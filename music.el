(require 'json)

(defvar play-state nil
  "Song play state, nil or t.")

(defvar play-list ()
  "Your Play List.")

(defvar play-list-start-position nil
  "Play list start position in netease-music buffer.")

(defvar songs-list ()
  "Songs list. A playlist's all songs, and you can add other song into it.")

(defconst api "http://localhost:3000"
  "API ADDRESS.")

(defconst login-url "/login/cellphone"
  "Login url pattern.")

(defconst playlist-url "/user/playlist"
  "Playlist url pattern.")

(defconst playlist-detail-url "/playlist/detail"
  "playlist detail url pattern.")

(defconst user-detail-url "/user/detail"
  "User detail url pattern.")

(defconst play-list-url "/user/playlist"
  "User playlist.")

(defconst song-url "/music/url"
  "Music real url.")

(defconst login-args "?phone=%s&password=%s"
 "Login args.")

(defconst user-detail-args "?uid=%s"
  "User detail args.")

(defconst playlist-args "?uid=%s"
  "Playlist args.")

(defconst playlist-detail-args "?id=%s"
  "Playlist detail args.")

(defconst song-args "?id=%s"
  "Song args.")

(defconst netease-music-title
  "* NetEase Music\n %s  等级：%s 听歌数：%s \n%s \n** 歌单列表 \n")

(defun format-netease-title ()
  (format netease-music-title
          (slot-value admin-ins 'name)
          (slot-value admin-ins 'level)
          (slot-value admin-ins 'listenSongs)
          (slot-value admin-ins 'description)))

(defclass admin ()
  ((name)
   (level)
   (listenSongs)
   (description)))

(defvar admin-ins (make-instance 'admin))

(defun format-user-detail (id)
  (let* ((json (request user-detail-url (format-user-detail-args id))))
    (setf (slot-value admin-ins 'name) (set-user-nickname json))
    (setf (slot-value admin-ins 'level) (set-user-level json))
    (setf (slot-value admin-ins 'listenSongs) (set-user-listenSongs json))
    (setf (slot-value admin-ins 'description) (set-user-description json))))

(defun format-login-args (phone password)
  "Format login args."
  (format login-args phone password))

(defun format-user-detail-args (uid)
  "Format user detail args."
  (format user-detail-args uid))

(defun format-playlist-args (uid)
  "Format playlist args."
  (format playlist-args uid))

(defun format-playlist-detail-args (id)
  "Format playlist detail args."
  (format playlist-detail-args id))

(defun format-song-args (id)
  "Format song args."
  (format song-args id))

(defvar user-id nil
  "User id.")

(defvar user-password nil
  "User password.")

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
(define-derived-mode netease-music-mode org-mode "netease-music-playing")
(define-derived-mode netease-music-mode org-mode "netease-music-playlist")

(defun netease-music-init ()
  (setq phone (read-string "Your Phone Number Please: "))
  (setq password (read-string "Your Password: "))
  (netease-music-login phone password)
  (init-frame))

(defun netease-music-login (username password)
  (let* ((json (request login-url (format-login-args phone password))))
    (setq user-id (set-user-id json))
    (format-user-detail user-id)))

(defun set-user-id (json)
  "Return user id from JSON."
  (cdr (assoc 'id (cdr (assoc 'account json)))))

(defun set-user-nickname (json)
  (cdr (assoc 'nickname (cdr (assoc 'profile json)))))

(defun set-user-level (json)
  (cdr (assoc 'level json)))

(defun set-user-listenSongs (json)
  (cdr (assoc 'listenSongs json)))

(defun set-user-description (json)
  (cdr (assoc 'description (cdr (assoc 'profile json)))))

(defun set-user-avatar-url (json)
  (cdr (assoc 'avatarUrl (cdr (assoc 'profile json)))))

;; (defun set-user-details (user-id)
;;   (let* ((json (request user-detail-url (format-user-detail-args user-id))))
;;     (setq nickname (set-user-nickname json))
;;     (setq avatar-url (set-user-avatar-url json))))

(defun request (url-pattern args)
  "Return json by request the url."
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

(defun get-playlist ()
  "Format playlist detail to a dict."
  (let* ((json (request play-list-url
                        (format-playlist-args user-id)))
         (detail (cdr (assoc 'playlist json))))
    (setq play-list ())
    (dotimes (i (length detail))
      (let* ((lst (aref detail i))
             (name (cdr (assoc 'name lst)))
             (list-id (cdr (assoc 'id lst)))
             (cell (cons name list-id)))
        (push cell play-list)))))

(defun get-playlist-tracks (json)
  "Get tracks from playlist."
  (cdr (assoc 'tracks (cdr (assoc 'result json)))))

(defun get-song-from-tracks (json index)
  "Get song details from tracks in playlist."
  (aref json index))

(defun get-playlist-detail (id)
  "Get playlist's songs."
  (let* ((json (request playlist-detail-url
                        (format-playlist-detail-args id)))
         (tracks (get-playlist-tracks json)))
    (setq songs-list ())
    (dotimes (index (length tracks))
      (setq song (get-song-from-tracks tracks index))
      (setq song-name (cdr (assoc 'name song)))
      (setq song-id (cdr (assoc 'id song)))
      (push (cons song-name song-id) songs-list))))

(defun get-song-real-url (id)
  "Return song's real url."
  (let* ((json (request song-url
                        (format-song-args id))))
    (cdr (assoc 'url (aref (cdr (assoc 'data json)) 0)))))

(defun init-frame ()
  "Initial main interface. When you first login netease-music list all your playlist."
  (erase-buffer)
  (insert (format-netease-title))
  (get-playlist)
  (insert (format-playlist-table play-list)))

(defun play-song (song-url)
  "Use EMMS to play songs."
  (emms-play-url song-url))

(defun play ()
  "Play current song with EMMS."
  (if (not play-state)
      (emms-play)))

(defun pause ()
  "Stop current song with EMMS."
  (if play-state (emms-stop)))

(defun format-playlist-table (playlist)
  "Format the user's all playlist."
  (let ((playlist-table ""))
    (dotimes (index (safe-length playlist) playlist-table)
      (setq playlist-table (concat playlist-table
              (format "%s\n" (car (elt playlist index))))))))

(defun format-playlist-songs-table (songs)
  "Format the playlist's all song."
  (let ((songs-table ""))
    (dotimes (index (safe-length songs) songs-table)
      (setq songs-table (concat songs-table
              (format "%s\n" (car (elt songs index))))))))

(defun find-playlist-id (playlist-name)
  "Return playlist id from play-list which contains the users' all playlist."
  (assoc-default playlist-name play-list))

(defun jump-into-playlist-buffer ()
  "Switch to the playlist buffer whose name is this line's content."
  (interactive)
  (setq playlist-name (get-current-line-content))
  (setq id (find-playlist-id playlist-name))
  (get-buffer-create "netease-music-playlist")
  (switch-to-buffer "netease-music-playlist")
  (netease-music-mode)
  (erase-buffer)
  (get-playlist-detail id)
  (erase-buffer)
  (insert (format-netease-title))
  (insert (format-playlist-songs-table songs-list)))

(defun find-song-id (song-name)
  "Return song id from songs-list which contains this playlist's all song."
  (assoc-default song-name songs-list))

(defun jump-into-song-buffer ()
  "Switch to the song's buffer whose name is this line's content."
  (interactive)
  (setq song-name (get-current-line-content))
  (setq id (find-song-id song-name))
  (get-buffer-create "netease-music-playing")
  (switch-to-buffer "netease-music-playing")
  (netease-music-mode)
  (erase-buffer)
  (setq song-url (get-song-real-url id))
  (play-song song-url)
  (erase-buffer)
  (insert (format-netease-title))
  (insert song-name))

(defun get-current-line-content ()
  "Return current line's content."
  (car (split-string
        (thing-at-point 'line t)
        "\n")))
