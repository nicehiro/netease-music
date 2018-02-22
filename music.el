;;; -*- lexical-binding: t; -*-

(require 'json)
(require 'url)
(require 'org)

(defgroup netease-music nil
  "Netease music plugin for Emacs."
  :prefix "netease-music-"
  :group 'music
  :link '(url-link :tag "Github" "https://github.com/nicehiro/netease-music"))

(defclass SONG ()
  ((name)
   (artist)
   (album)
   (song-id)))

(defclass PLAYLIST ()
  ((name)
   (id)
   (description)
   (user-id)))

(defclass admin ()
  ((name)
   (level)
   (listenSongs)
   (description)))

(define-namespace netease-music-

(defcustom username nil
  "Your netease music username."
  :type 'string)

(defcustom password nil
  "Your netease music password."
  :type 'string)

(defconst buffer-name-search "Search Results"
  "Search window buffer's name.")

(defvar play-list ()
  "Your Play List.")

(defvar current-playing-song (make-instance 'SONG)
  "This is current playing SONG.")

(defun format-current-playing-song (name artist album song-id)
  "Format current playing song."
  (setf (slot-value current-playing-song 'name) name)
  (setf (slot-value current-playing-song 'artist) artist)
  (setf (slot-value current-playing-song 'album) album)
  (setf (slot-value current-playing-song 'song-id) song-id))

(defvar songs-list ()
  "Songs list. A playlist's all songs, and you can add other song into it.")

(defconst api "http://119.23.207.231:3000"
  "NetEase Music API ADDRESS.")

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

(defconst personal-fm-url "/personal_fm"
  "Personal f.m. url.")

(defconst search-url "/search"
  "Search url.")

(defconst like-url "/like"
  "I like it url.")

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

(defconst search-args "?keywords=%s"
  "Search args.")

(defconst like-args "?id=%s"
  "I like it args.")

(defun format-lyric-args (song-id)
  "Format lyric args."
  (format lyric-args song-id))

(defun format-like-args (song-id)
  (format like-args song-id))

(defconst netease-music-title
  "* NetEase Music\n %s  等级：%s 听歌数：%s \n私人FM\n%s \n** %s \n%s \n")

(defun format-netease-title (banner-string description)
  "Format netease title."
  (format netease-music-title
          (slot-value admin-ins 'name)
          (slot-value admin-ins 'level)
          (slot-value admin-ins 'listenSongs)
          (slot-value admin-ins 'description)
          banner-string
          description))

(defun netease-music-playlist-description (playlist-name)
  (find-playlist-description playlist-name))

(defun set-song-name (tracks)
  "Return song name about this song."
  (cdr (assoc 'name tracks)))

(defun set-song-id (tracks)
  "Return song id about this song."
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
  (cdr (assoc 'name (assoc 'album tracks))))

(defun format-song-detail (tracks instance)
  "Format SONG instance."
    (setf (slot-value instance 'name) (set-song-name tracks))
    (setf (slot-value instance 'song-id) (set-song-id tracks))
    (setf (slot-value instance 'artist) (set-artist-name tracks))
    (setf (slot-value instance 'album) (set-album-name tracks)))

;;; Class PLAYLIST start here.
  

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
  (make-instance 'admin)
  "When you login will create a user instance.")

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

(defun format-search-args (keyword)
  "Format search args."
  (format search-args keyword))

(defvar user-id nil
  "User id.")

(defvar user-password nil
  "User password.")

(defvar avatar-url nil
  "User avatar url.")

(defun format-request-url (url args)
  "Format request url."
  (url-unhex-string (concat api url args)))

(defun start ()
  (interactive)
  (init))

(define-derived-mode mode org-mode "netease-music"
  "Key bindings of netease-music-mode."
  (evil-define-key
    'normal
    netease-music-mode-map
    (kbd "RET")
    'netease-music-jump-into)
  (evil-define-key
    'normal
    netease-music-mode-map
    (kbd "l")
    'netease-music-i-like-it)
  (evil-define-key
    'normal
    netease-music-mode-map
    (kbd "n")
    'netease-music-play-next)
  (evil-define-key
    'normal
    netease-music-mode-map
    (kbd "p")
    'netease-music-pause)
  (evil-define-key
    'normal
    netease-music-mode-map
    (kbd "q")
    'quit-window))

(defun init ()
  "Initialize netease music information."
  (login netease-music-username netease-music-user-password)
  (init-frame))

(defun login (username password)
  "Login netease music."
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
        (push (cons name playlist-ins) play-list))))
  (setq play-list (reverse-list play-list)))

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
      (push (cons song-name song-ins) songs-list)))
  (setq songs-list (reverse-list songs-list)))

(defun search ()
  "Search songs. Multiple keywords can be separated by SPC."
  (interactive)
  (setq keywords (read-string "Please input the keywords you want to search: "))
  (setq search-songs ())
  (let* ((json (request search-url
                        (format-search-args keywords)))
         (songs (cdr (assoc 'songs (cdr (assoc 'result json)))))
         (count (length songs)))
    (dotimes (index count search-songs)
      (let* ((song (get-song-from-tracks songs index))
             (song-name (cdr (assoc 'name song)))
             (song-ins (make-instance 'SONG)))
        (format-song-detail song song-ins)
        (push (cons song-name song-ins) search-songs)))
    (setq current-config (current-window-configuration))
    ;;; popup window
    (popwin:popup-buffer (get-buffer-create buffer-name-search))
    (switch-to-buffer buffer-name-search)
    (erase-buffer)
    (mode))
    (insert (format-netease-title "Search Results: "
                                "Press jump-into to listen the song.\nPress add-to-songslist can add to the songs list."))
    (insert "*** Song List:\n")
    (insert (format-playlist-songs-table search-songs)))

(defun get-lyric (song-id)
  "Return lyric of current song."
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

(defun get-personal-fm ()
  "Get personal f.m. songs."
  (let* ((json (request personal-fm-url ""))
         (data (cdr (assoc 'data json))))
    (setq songs-list ())
    (dotimes (index (length data))
      (setq song (aref data index))
      (setq song-name (cdr (assoc 'name song)))
      (setq song-ins (make-instance 'SONG))
      (format-song-detail song song-ins)
      (push (cons song-name song-ins) songs-list))))

(defun init-frame ()
  "Initial main interface. When you first login netease-music list all your playlist."
  (interactive)
  (switch-to-buffer "netease-music")
  (mode)
  (erase-buffer)
  (insert (format-netease-title "Signature:"
                                (find-admin-description)))
  (get-playlist)
  (insert "\n*** 歌单列表\n")
  (insert (format-playlist-table play-list)))

(defun play-song (song-url)
  "Use EMMS to play songs."
  (emms-play-url song-url))

(defun play ()
  "Play song."
  (interactive)
  (emms-start))

(defun pause ()
  "Stop song."
  (interactive)
  (emms-stop))

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
  (get-playlist-detail id)
  (with-current-buffer "netease-music-playlist"
    (erase-buffer)
    (mode)
    (insert (format-netease-title playlist-name
                                  (find-playlist-description playlist-name)))
    (insert "*** Song List:\n")
    (insert (format-playlist-songs-table songs-list))
    (goto-char (point-min))))

(defun find-song-id (song-name)
  "Find song's id of current song."
  (setq song-ins (assoc-default song-name songs-list))
  (slot-value song-ins 'song-id))

(defun find-song-album (song-name)
  "Find song's album of current song."
  (setq song-ins (assoc-default song-name songs-list))
  (slot-value song-ins 'album))

(defun find-song-artist (song-name)
  "Find song's artist of current song."
  (setq song-ins (assoc-default song-name songs-list))
  (slot-value song-ins 'artist))

(defun find-song-item (item song-name)
  (setq song-ins (assoc-default song-name songs-list))
  (slot-value song-ins 'item))

(defmacro find-song (item)
  `(defun ,(intern (format "find-song-%s" item)) ()
     ,(format "Find %s in song." item)
     (find-song-item ,item song-name)))

(defun jump-into-song-buffer ()
  "Switch to the song's buffer whose name is this line's content."
  (setq song-name (get-current-line-content))
  (play-song-by-name song-name))

(defun play-song-by-name (song-name)
  "Play a song by the name."
  (setq id (find-song-id song-name))
  (setq album (find-song-album song-name))
  (setq artist (find-song-artist song-name))
  (get-buffer-create "netease-music-playing")
  (setq song-url (get-song-real-url id))
  (play-song song-url)
  (format-current-playing-song song-name artist album id)
  (with-current-buffer "netease-music-playing"
    (erase-buffer)
    (mode)
    (insert (format-netease-title song-name
                                  (format "Artist: %s  Album: %s" artist album)))
    (insert (get-lyric id))
    (goto-char (point-min)))
  (with-current-buffer "netease-music-playlist"
    (goto-char (point-min))
    (search-forward (slot-value current-playing-song 'name))))

(defun jump-into-personal-fm ()
  "Jump into your personal fm songs list."
  (get-personal-fm)
  (with-current-buffer "netease-music-playlist"
    (erase-buffer)
    (mode)
    (insert (format-netease-title "私人FM"
                                  "你的私人 FM 听完之后再次请求可以获得新的歌曲"))
    (insert "*** Song List:\n")
    (insert (format-playlist-songs-table songs-list))
    (goto-char (point-min))))

(defun jump-into ()
  "Jump into next buffer based on this line's content."
  (interactive)
  (eval-buffer "music.el")
  (let* ((current-buffer-name (buffer-name)))
    (cond ((equal (get-current-line-content) "私人FM")
           (message "私人FM")
           (jump-into-personal-fm))
          ((equal current-buffer-name "netease-music")
           (jump-into-playlist-buffer))
          ((equal current-buffer-name "netease-music-playlist")
           (jump-into-song-buffer)))))

;;; emms 播放完当前曲目之后自动播放下一首
;;; when emms finished current song's play, auto play next song.
(add-hook 'emms-player-finished-hook 'netease-music-play-next)
(add-hook 'emms-player-finished-hook 'netease-music-mode-line-format)

;;; 这里的函数写的太丑了！！！可是又没有什么好办法现在……
(defun play-next ()
  "Return next song name in songs-list."
  (interactive)
  (eval-buffer "music.el")
  (let* ((current-playing-song-name (slot-value current-playing-song 'name))
         (next-song-name current-playing-song-name)
         (can-play nil)
         (count (length songs-list))
         (position 0))
    (dotimes (index count next-song-name)
      (let* ((block (nth index songs-list))
             (song (cdr block))
             (song-name (slot-value song 'name)))
        (if (and (equal song-name current-playing-song-name)
                 (< index (- count 1)))
            (progn
              (setq can-play 1)
              (setq position index)))
        (setq next-song-name
                  (slot-value (cdr (nth (+ position 1) songs-list))
                              'name))))
    (message next-song-name)
    (if can-play
        (play-song-by-name next-song-name))
    (mode-line-format)))

(defun add-to-songslist (song)
  "Add song to songs-list."
  (interactive)
  (let ((name (slot-value song 'name)))
    (push (cons name song) songs-list)))

(defun get-current-line-content ()
  "Return current line's content."
  (car (split-string
        (thing-at-point 'line t)
        "\n")))

(defun reverse-list (lst)
  "Reverse list."
  (do ((a lst b)
       (b (cdr lst) (cdr b))
       (c nil a))
    ((atom a) c)
    (rplacd a c)))

(defun i-like-it ()
  (interactive)
  (request like-url
           (format-like-args (slot-value
                              (cdr (assoc (slot-value current-playing-song 'name) songs-list))
                              'song-id)))
  (message "Add to your favorite playlist!"))

(defun mode-line-format ()
  (setq emms-mode-line-format (slot-value netease-music-current-playing-song 'name))
  (emms-mode-line-alter-mode-line)))

(provide 'music)
