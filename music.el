(require 'json)

(defvar play-state nil
  "Song play state, nil or t.")

(defvar play-list ()
  "Your Play List.")

(defvar current-playing nil
  "Your current playing song.")

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

(defconst lyric-url "/lyric"
  "Lyric url.")

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

(defconst lyric-args "?id=%s"
  "Lyric args.")

(defun format-lyric-args (song-id)
  "Format lyric args."
  (format lyric-args song-id))

(defconst netease-music-title
  "* NetEase Music\n %s  等级：%s 听歌数：%s \n%s \n** %s \n%s \n")

(defun format-netease-title (banner-string description)
  (format netease-music-title
          (slot-value admin-ins 'name)
          (slot-value admin-ins 'level)
          (slot-value admin-ins 'listenSongs)
          (slot-value admin-ins 'description)
          banner-string
          description))

(defun netease-music-playlist-description (playlist-name)
  (find-playlist-description playlist-name))

(defclass SONG ()
  ((name)
   (artist)
   (album)
   (song-id)))

(defun set-song-name (tracks)
  (cdr (assoc 'name tracks)))

(defun set-song-id (tracks)
  (cdr (assoc 'id tracks)))

(defun set-artist-name (tracks)
  "Return artist name about this song."
  (let* ((count (length (cdr (assoc 'artists tracks))))
         (artist-name ""))
    (dotimes (index count artist-name)
      (setq name (cdr (assoc 'name (aref (cdr (assoc 'artists tracks)) index))))
      (message name)
      (setq artist-name (concat name "  " artist-name)))))

(defun set-album-name (tracks)
  "Return album name about this song."
  (let* ((count (length (cdr (assoc 'albun tracks))))
         (track-name ""))
    (dotimes (index count track-name)
      (setq name (cdr (assoc 'name (aref (cdr (assoc 'album tracks)) index))))
      (setq track-name (concat track-name name)))))

(defun format-song-detail (tracks instance)
    (setf (slot-value instance 'name) (set-song-name tracks))
    (setf (slot-value instance 'song-id) (set-song-id tracks))
    (setf (slot-value instance 'artist) (set-artist-name tracks))
    (setf (slot-value instance 'album) (set-album-name tracks)))
  
(defclass PLAYLIST ()
  ((name)
   (id)
   (description)
   (user-id)))

(defun set-playlist-name (json)
  (cdr (assoc 'name json)))

(defun set-playlist-description (json)
  (let ((description (cdr (assoc 'description json))))
    (if (equal description nil)
        "暂无歌单简介"
      description)))

(defun set-playlist-userid (json)
  (cdr (assoc 'userId json)))

(defun format-playlist-detail (instance json id)
    (setf (slot-value instance 'user-id) (set-playlist-userid json))
    (setf (slot-value instance 'name) (set-playlist-name json))
    (setf (slot-value instance 'description) (set-playlist-description json))
    (setf (slot-value instance 'id) id))

;;; User Details Start Here.

(defclass admin ()
  ((name)
   (level)
   (listenSongs)
   (description)))

(defun set-user-id (json)
  "Return user's id from JSON."
  (cdr (assoc 'id (cdr (assoc 'account json)))))

(defun set-user-nickname (json)
  "Return user's nickname."
  (cdr (assoc 'nickname (cdr (assoc 'profile json)))))

(defun set-user-level (json)
  "Return user's netease-music level."
  (cdr (assoc 'level json)))

(defun set-user-listenSongs (json)
  "Retutn user's listensongs count."
  (cdr (assoc 'listenSongs json)))

(defun set-user-description (json)
  "Return user's description. Default is nil."
  (cdr (assoc 'description (cdr (assoc 'profile json)))))

(defun set-user-avatar-url (json)
  "Return user's avatar-url."
  (cdr (assoc 'avatarUrl (cdr (assoc 'profile json)))))

(defvar admin-ins
  (make-instance 'admin))

(defun format-user-detail (id)
  "Initialize user details."
  (let* ((json (request user-detail-url (format-user-detail-args id))))
    (setf (slot-value admin-ins 'name) (set-user-nickname json))
    (setf (slot-value admin-ins 'level) (set-user-level json))
    (setf (slot-value admin-ins 'listenSongs) (set-user-listenSongs json))
    (setf (slot-value admin-ins 'description) (set-user-description json))))

;;; User Details Ends Here.

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

(defun netease-music-init ()
  (setq phone (read-string "Your Phone Number Please: "))
  (setq password (read-string "Your Password: "))
  (netease-music-login phone password)
  (init-frame))

(defun netease-music-login (username password)
  (let* ((json (request login-url (format-login-args phone password))))
    (setq user-id (set-user-id json))
    (format-user-detail user-id)))

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
             (playlist-ins (make-instance 'PLAYLIST))
             (list-id (cdr (assoc 'id lst)))
             (name (cdr (assoc 'name lst))))
        (format-playlist-detail playlist-ins lst list-id)
        (push (cons name playlist-ins) play-list)))))

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
      (setq song-ins (make-instance 'SONG))
      (format-song-detail song song-ins)
      (push (cons song-name song-ins) songs-list))))

(defun get-lyric (song-id)
  (let* ((json (request lyric-url
                        (format-lyric-args song-id)))
         (lrc (cdr (assoc 'lrc json)))
         (lyric (cdr (assoc 'lyric lrc))))
    lyric))

(defun get-song-real-url (id)
  "Return song's real url."
  (let* ((json (request song-url
                        (format-song-args id))))
    (cdr (assoc 'url (aref (cdr (assoc 'data json)) 0)))))

(defun init-frame ()
  "Initial main interface. When you first login netease-music list all your playlist."
  (erase-buffer)
  (insert (format-netease-title "Signature:"
                                (find-admin-description)))
  (get-playlist)
  (insert "\n*** 歌单列表\n")
  (insert (format-playlist-table play-list)))

(defun play-song (song-url)
  "Use EMMS to play songs."
  (emms-play-url song-url))

(defun play-songslist ()
  )
(add-hook 'emms-player-finished-hook 'play-next)

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

(defun find-admin-description ()
  (slot-value admin-ins 'description))

(defun find-playlist-id (playlist-name)
  "Return playlist id from play-list which contains the users' all playlist."
  (setq playlist-ins (assoc-default playlist-name play-list))
  (slot-value playlist-ins 'id))

(defun find-playlist-description (playlist-name)
  (setq playlist-ins (assoc-default playlist-name play-list))
  (slot-value playlist-ins 'description))

(defun jump-into-playlist-buffer ()
  "Switch to the playlist buffer whose name is this line's content."
  (setq playlist-name (get-current-line-content))
  (setq id (find-playlist-id playlist-name))
  (get-buffer-create "netease-music-playlist")
  (switch-to-buffer "netease-music-playlist")
  (netease-music-mode)
  (get-playlist-detail id)
  (erase-buffer)
  (insert (format-netease-title playlist-name 
                                (find-playlist-description playlist-name)))
  (insert "*** Song List:\n")
  (insert (format-playlist-songs-table songs-list)))

(defun find-song-id (song-name)
  (setq song-ins (assoc-default song-name songs-list))
  (slot-value song-ins 'song-id))

(defun find-song-album (song-name)
  (setq song-ins (assoc-default song-name songs-list))
  (slot-value song-ins 'album))

(defun find-song-artist (song-name)
  (setq song-ins (assoc-default song-name songs-list))
  (slot-value song-ins 'artist))

(defun jump-into-song-buffer ()
  "Switch to the song's buffer whose name is this line's content."
  (setq song-name (get-current-line-content))
  (play-song-by-name song-name))

(defun play-song-by-name (song-name)
  (setq id (find-song-id song-name))
  (setq album (find-song-album song-name))
  (setq artist (find-song-artist song-name))
  (get-buffer-create "netease-music-playing")
  (switch-to-buffer "netease-music-playing")
  (netease-music-mode)
  (setq song-url (get-song-real-url id))
  (play-song song-url)
  (setq current-playing song-name)
  (erase-buffer)
  (insert (format-netease-title song-name (format "%s  %s" artist album)))
  (insert (get-lyric id)))

(defun jump-into ()
  (interactive)
  (let* ((current-buffer-name (buffer-name)))
    (if (equal current-buffer-name "netease-music")
        (jump-into-playlist-buffer)
      (jump-into-song-buffer))))

(defun play-next ()
  (interactive)
  (setq next-song-name current-playing)
  (let* ((count (length songs-list)))
    (dotimes (index count next-song-name)
      (let* ((block (nth index songs-list))
             (song (cdr block))
             (song-name (slot-value song 'name)))
        (if (equal song-name current-playing)
               (setq next-song-name
                  (slot-value (cdr (nth (+ index 1) songs-list))
                              'name))))))
  (play-song-by-name next-song-name))

(defun add-to-songslist (song)
  (interactive)
  (let ((name (slot-value song 'name)))
    (push (cons name song) songs-list)))

(defun get-current-line-content ()
  "Return current line's content."
  (car (split-string
        (thing-at-point 'line t)
        "\n")))
