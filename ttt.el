;;; ttt.el --- Tic-Tac-Toe game

;; Author: KrokodileGlue <KrokodileGlue@outlook.com>
;; Keywords: game games two-player inane diversion
;; Version: 0.1beta
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

;;; Code:

(defvar ttt-board nil)
(defvar ttt-player nil)

(defvar ttt-x-score nil)
(defvar ttt-o-score nil)

(define-derived-mode ttt-mode special-mode "ttt"
  (define-key ttt-mode-map (kbd "RET") 'ttt-mark))

;; Board functions.

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
        (if (eq ttt-player ?\X) ?\O ?\X)))

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
  (setq ttt-player ?\X)

  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (format "X score: %d\n" ttt-x-score))
    (insert (format "O score: %d\n" ttt-o-score))
    (dotimes (x 3)
      (dotimes (y 3)
        (insert (ttt-get x y)))
      (insert "\n")))
  (goto-char (point-min))
  (forward-line)
  (forward-line))

;; User functions.

(defun ttt-mark ()
  "Mark a square on the board."
  (interactive)

  (if (or (> (current-column) 2)
          (> (- (line-number-at-pos) 2) 3)
          (< (line-number-at-pos) 3))
      (message "You must mark a square.")

    (if (ttt-is-player
         (ttt-get (- (line-number-at-pos) 3)
                  (current-column)))
        (message "You may not mark an already-marked square.")

      (ttt-set
       (- (line-number-at-pos) 3)
       (current-column)
       ttt-player)

      (let ((inhibit-read-only t))
        (delete-char 1)
        (insert ttt-player)
        (backward-char 1))

      (if (ttt-full)
          (progn
            (message "Tie!")
            (ttt-init))
        (when (ttt-winning)
          (if (char-equal ttt-player ?\X)
              (setq ttt-x-score (+ 1 ttt-x-score))
            (setq ttt-o-score (+ 1 ttt-o-score)))
          (message "Player %c has won this round." ttt-player)
          (if (or (>= (+ ttt-x-score ttt-o-score) 3)
                  (= ttt-x-score 2)
                  (= ttt-o-score 2))
              (ttt-game-over)
            (ttt-init))))

      (ttt-swap-players))))

(defun ttt-game-over ()
  "Notify the user of who won."
  (kill-buffer)
  (message "Player %c has won!"
           (if (> ttt-x-score ttt-o-score) ?\X ?\O)))

(defun ttt ()
  "Start a game of Tic-Tac-Toe."
  (interactive)
  (switch-to-buffer "*tic-tac-toe*")
  (ttt-mode)
  (setq ttt-x-score 0)
  (setq ttt-o-score 0)
  (ttt-init)
  (message "Welcome to Tic-Tac-Toe!"))

(provide 'ttt)

;;; ttt.el ends here
