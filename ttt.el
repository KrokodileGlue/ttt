;;; ttt.el --- Tic-Tac-Toe game

;; Author: KrokodileGlue <KrokodileGlue@outlook.com>
;; Keywords: game games two-player inane diversion
;; Version: 0.5beta
;; URL: https://github.com/KrokodileGlue/ttt

;; This file is not part of GNU Emacs.

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:
;; This package provides a command and major mode for playing
;; two-player games of tic-tac-toe.

;; TODO:
;; - Handle server death cleanly.
;; - Implement a bot to play against.

;;; Code:

(require 'cl-lib)			; For cl-defun/return-from.

;;; Variables.

(defvar ttt-board nil)
(defvar ttt-buffer "*tic-tac-toe*")
(defvar ttt-player nil)			; The current player's symbol.
(defvar ttt-local-player nil)		; The local player's symbol.

(defvar ttt-x-score nil)
(defvar ttt-o-score nil)

;; Two states: "menu" and "game".
(defvar ttt-state nil)

;; Three modes: "local", "network", and "bot".
(defvar ttt-mode nil)

;; Network variables.

(defvar ttt-server-clients '())
(defvar ttt-client-connection-timeout 3)
(defvar ttt-server-name "ttt-server")
(defvar ttt-client-name "ttt-client")
(defvar ttt-port 10000)
(defvar ttt-server nil)

(defvar ttt-client-connection nil)
(defvar ttt-is-host nil)
(defvar ttt-timer nil)

(define-derived-mode ttt-mode special-mode "ttt"
  (define-key ttt-mode-map (kbd "RET") 'ttt-mark))

;;; Server code.

(defun ttt-server-start ()
  "Start the ttt multi-player server."
  (setq ttt-server-clients '())
  (setq ttt-server
	(make-network-process
	 :name      ttt-server-name
	 :service   ttt-port
	 :buffer    ttt-buffer
	 :filter   'ttt-server-filter
	 :sentinel 'ttt-server-sentinel
	 :server    t
	 :family   'ipv4)))

(defun ttt-server-stop ()
  "Stop the ttt multi-player server."
  (while ttt-server-clients
    (delete-process (car (car ttt-server-clients)))
    (setq ttt-server-clients (cdr ttt-server-clients)))
  (delete-process ttt-server))

(defun ttt-server-sentinel (proc msg)
  "Catch change in @PROC connection as @MSG."
  (when (string= msg "connection broken by remote peer\n")
    (message "Your opponent has quit." proc)))

(cl-defun ttt-server-filter (proc s)
  "Receive incoming messages from @PROC as @S."
  (unless (assoc proc ttt-server-clients) ; We have a new client!
    (setq ttt-server-clients (cons (cons proc "") ttt-server-clients))
    (message "New tic-tac-toe client from %s!" proc))
  (when (equal s "getboard")
    (process-send-string
     proc
     (with-current-buffer ttt-buffer
       (buffer-string)))
    (cl-return-from ttt-server-filter))
  (let ((p (- (string-to-number s) 42)))
    (aset ttt-board
	  (if (> p 6)
	      (- p 2)
	    (if (> p 2)
		(- p 1)
	      p))
	  ttt-player))
  (unless (ttt-check-winning-state)
    (ttt-swap-players)
    (ttt-render)))

(defun ttt-client-connect (addr)
  "Open a tic-tac-toe connection as a client with @ADDR."
  (setq ttt-client-connection
	(make-network-process
	 :name      ttt-client-name
	 :service   ttt-port
	 :host      addr
	 :buffer    ttt-buffer
	 :sentinel 'ttt-client-sentinel
	 :family   'ipv4)))

(defun ttt-client-sentinel (proc s)
  "Receive connection change messages from @PROC as @S."
  (unless (or (string= s "open\n")
	      (string-prefix-p "open from " s))
    (cancel-timer ttt-timer)
    (ttt-client-disconnect)
    (ttt-game-over)))

(defun ttt-client-disconnect ()
  "Disconnect from the tic-tac-toe host."
  (delete-process ttt-client-connection))

(defun ttt-client-send (s)
  "Send @S to the tic-tac-toe host."
  (process-send-string ttt-client-connection s))

(defun ttt-client-update-board ()
  "Update the client board by asking the host for it."
  (with-current-buffer ttt-buffer
    (let ((inhibit-read-only t) (point (point)))
      (erase-buffer)
      (ttt-client-send "getboard")
      (accept-process-output
       ttt-client-connection
       ttt-client-connection-timeout)
      (goto-char (point-min))
      (re-search-forward "Current player: ")
      (setq ttt-player (char-after))
      (goto-char (point-min))
      (re-search-forward "X score: ")
      (setq ttt-x-score (string-to-number (char-to-string (char-after))))
      (goto-char (point-min))
      (re-search-forward "O score: ")
      (setq ttt-o-score (string-to-number (char-to-string (char-after))))
      (goto-char point))))

;;; Gameplay code.

;; Board functions.

(defun ttt-render ()
  "Render the board into `ttt-buffer`."
  (with-current-buffer ttt-buffer
    (let ((inhibit-read-only t)
	  (point (point)))
      (erase-buffer)
      (insert (format "Play mode: %s\n" ttt-mode))
      (insert (format "X score: %d\n" ttt-x-score))
      (insert (format "O score: %d\n" ttt-o-score))
      (dotimes (x 3)
	(dotimes (y 3)
	  (insert-char (ttt-get x y)))
	(insert "\n"))
      (insert (format "Current player: %c\n" ttt-player))
      (goto-char point))))

(defun ttt-get (x y)
  "Fetch the (@X,@Y) cell from the board."
  (elt ttt-board (+ y (* x 3))))

(defun ttt-set (x y v)
  "Set the (@X,@Y) cell to @V."
  (aset ttt-board (+ y (* x 3)) v))

(defun ttt-diagonal-win ()
  "Determine the state of diagonal wins."
  (or (ttt-same
       (ttt-get 0 0)
       (ttt-get 1 1)
       (ttt-get 2 2))
      (ttt-same
       (ttt-get 0 2)
       (ttt-get 1 1)
       (ttt-get 2 0))))

(defun ttt-row-win ()
  "Determine the state of horizontal wins."
  (let ((has-won nil))
    (dotimes (current-row 3)
      (when (ttt-same
             (ttt-get current-row 0)
             (ttt-get current-row 1)
             (ttt-get current-row 2))
        (setq has-won t)))
    has-won))

(defun ttt-col-win ()
  "Determine the state of vertical wins."
  (let ((has-won nil))
    (dotimes (current-column 3)
      (when (ttt-same (ttt-get 0 current-column)
                      (ttt-get 1 current-column)
                      (ttt-get 2 current-column))
        (setq has-won t)))
    has-won))

(defun ttt-full ()
  "Determine whether the board is fully marked or not."
  (let ((full t))
    (dotimes (x 3)
      (dotimes (y 3)
        (when (null (ttt-is-player (ttt-get x y)))
          (setq full nil))))
    full))

;; Game state functions.

(defun ttt-swap-players ()
  "Swap player from X to O, otherwise O to X."
  (setq ttt-player
        (if (char-equal ttt-player ?\X) ?\O ?\X)))

(defun ttt-same (x y z)
  "Whether squares @X, @Y, and @Z are all owned by the same player."
  (and (ttt-is-player x)
       (char-equal x y)
       (char-equal y z)))

(defun ttt-is-player (s)
  "Determine whether @S is a player."
  (or (char-equal s ?\X)
      (char-equal s ?\O)))

(defun ttt-winning ()
  "Determine whether someone has won the round."
  (or (ttt-row-win)
      (ttt-col-win)
      (ttt-diagonal-win)))

(defun ttt-init ()
  "Start a new round."
  (setq ttt-board (make-vector 9 ?\.))
  (ttt-render)
  (goto-char (point-min))
  (forward-line)
  (forward-line)
  (forward-line))

(defun ttt-check-winning-state ()
  "Check for ties/wins and adjust scores appropriately."
  (let ((w (or (ttt-winning) (ttt-full))))
    (if (ttt-full)
	(progn
	  (message "Tie!")
	  (ttt-init))
      (when (ttt-winning)
	(if (char-equal ttt-player ?\X)
	    (setq ttt-x-score (+ 1 ttt-x-score))
	  (setq ttt-o-score (+ 1 ttt-o-score)))
	(if (or (>= (+ ttt-x-score ttt-o-score) 3)
		(= ttt-x-score 2)
		(= ttt-o-score 2))
	    (ttt-game-over)
	  (ttt-init))))
    w))

;; User functions.

(cl-defun ttt-mark ()
  "Mark a square on the board or select a menu item."
  (interactive)

  (when (and
	 (string= ttt-mode "network")
	 (not (char-equal ttt-player ttt-local-player)))
    (message "You are waiting for your opponent to make a move.")
    (cl-return-from ttt-mark))

  ;; We might be looking at the menu...

  (when (equal ttt-state "menu")
    (setq ttt-mode
	  (if (equal (line-number-at-pos) 3) "local"
	    (if (equal (line-number-at-pos) 4) "network"
	      "bot")))
    (setq ttt-state "game")
    (when (equal ttt-mode "network")
      (setq ttt-is-host (y-or-n-p "Are you the host? "))
      (if ttt-is-host
	  (ttt-server-start)
	(setq ttt-local-player ?\O)
	(ttt-client-connect (read-string
			     "Enter the host's address (default 127.0.0.1): "
			     nil
			     nil
			     "127.0.0.1"))
	(setq ttt-timer (run-at-time "1 sec" 1 'ttt-client-update-board))
	(add-hook 'kill-buffer-hook (lambda () (cancel-timer ttt-timer)) t t)))
    (ttt-init)
    (cl-return-from ttt-mark))

  (when (ttt-is-player
	 (ttt-get (- (line-number-at-pos) 4)
		  (current-column)))
    (message "You may not mark an already-marked square.")
    (cl-return-from ttt-mark))

  (when (or (> (current-column) 2)
	    (> (- (line-number-at-pos) 3) 3)
	    (< (line-number-at-pos) 3))
    (message "You must mark a square.")
    (cl-return-from ttt-mark))

  ;; Otherwise we're marking a square (and are allowed to do so).

  (when (equal ttt-mode "network")
    (unless ttt-is-host
      (ttt-client-send (number-to-string (point)))
      (ttt-swap-players)
      (cl-return-from ttt-mark)))

  (ttt-set
   (- (line-number-at-pos) 4)
   (current-column)
   ttt-player)

  (unless (ttt-check-winning-state)
    (ttt-swap-players)
    (ttt-render)))

(defun ttt-game-over ()
  "Notify the user of who won."
  (when (and (string= ttt-mode "network")
	     ttt-is-host)
    (ttt-server-stop))
  (kill-buffer)
  (message "Player %c has won!"
	   (if (> ttt-x-score ttt-o-score) ?\X ?\O)))

(defun ttt-menu ()
  "Present a menu allowing the user to choose whatever gameplay mode they wish to play."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert "Welcome to Tic-Tac-Toe!\n")
    (insert "Select a mode:\n")
    (insert "1. Local two-player\n")
    (insert "2. Networked two-player\n")
    (insert "3. Single-player\n"))
  (goto-char (point-min))
  (forward-line)
  (forward-line))

(defun ttt ()
  "Start a game of Tic-Tac-Toe."
  (interactive)
  (switch-to-buffer ttt-buffer)
  (ttt-mode)
  (setq ttt-x-score 0)
  (setq ttt-o-score 0)
  (setq ttt-local-player ?\X)
  (setq ttt-player ?\X)
  (setq ttt-state "menu")
  (ttt-menu))

(provide 'ttt)

;;; ttt.el ends here
