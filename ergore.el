;;; ergore.el --- Personal Bitlbee robot and helper

;; Copyright (C) 2012 Jonathan Arkell

;; Author: Jonathan Arkell <jonnay@jonnay.net>
;; Created: 5 Oct 2012
;; Keywords: erc bitlbee bot
;; Version 0.1

;; This file is not part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary: 
;; Please see the org-file that this was generated from.

(defconst ergore-invocation "ERGORE!")

(defconst ergore-invocation "ERGORE!")

(defcustom ergore-commands 
  (append '((test . ergore-command-test)
            (info . ergore-command-info)
            (help . ergore-command-help)
            (seen . ergore-command-seen)
            (time . ergore-command-seen)
            (why  . ergore-command-why)
            (poke . ergore-command-force))

          (when (featurep 'sauron)
            '((ping . ergore-command-ping)))

          (when (featurep 'mindwave-emacs)
            '((brain . ergore-command-brain)))

          (when (featurep 'emagician-starter-kit)
            '((chao . ergore-command-chao))))

  "An associative list of command names and functions to call in the format of:
((command-name-symbol  . (lambda () ...))
 (command-name-symbol2 . 'funname))"
  :type '(alist :key-type symbol :value-type function))

(defvar ergore-debug t)

(defun ergore-command-test (data &rest args)
  "Test command.  Outputs to the Emacs log.  NO one will see it."
  (message "Data: %S Args: %S" data args))

(defun ergore-get-nick (data)
  "Retrieve the nickname from an erc-response structure."
  (car (erc-parse-user (erc-response.sender data))))

(defun ergore-send (sendee lines)
  "Send LINES to SENDEE.
SENDEE should be in the format of erc-response.sender 
LINES should be a string or a list of strings, which are the lines to send to the user."
  (let ((nick (car (erc-parse-user sendee))))
    (cond ((stringp lines)
           (erc-message "PRIVMSG" (concat "&bitlbee " nick ": " lines) nil))
          ((listp lines)
           (mapc (lambda (line)
                   (erc-message "PRIVMSG" (concat "&bitlbee " nick ": " line) nil))
                 lines)))))

(defun ergore-command-help (data &rest args)
  "List all available commands."
  (ergore-send (erc-response.sender data)
               (append '("here are the commands I accept:")
                       (mapcar (lambda (command)
                                 (format "ERGORE! %s - %s"
                                         (car command)
                                         (documentation (cdr command) t)))
                               ergore-commands))))

(defun ergore-command-info (data &rest args)
  "Basic info about ERGORE."
  (ergore-send (erc-response.sender data) 
               (list "Hello.  I am Ergore.  You can interact with me, and I can things for you--and especially--for my MASTER."
                     "Say 'ERGORE! help' for a list of commands.  Say 'ERGORE! ping' to get Jonnays attention.")))

(defun ergore-command-why (data &rest args)
  "Learn why Jonnay made this and how"
  (ergore-send (ergore-get-nick data)
               (list "Like Dr. Frankenstein, Jonnay made me in his Emacs laboratory one night.  He wasn't wearing his lab-coat at the time, but he sure wishes he was."
                     "I was made because people sometimes need to contact Jonnay in the middle of a deep coding session.  Since Jonnay uses Emacs (his code editor) as his IM client, sometimes he misses messages because he is in the state of flow."
                     "I am named Ergore for a few reasons.  First, it sounds like 'Igor'.  It has 'Er' in front because the client jonnay uses is called 'ERC'.  It also is reminiscent of 'ermahgerd' (http://knowyourmeme.com/memes/ermahgerd)"
                     "Now you know.  ERMAHGERD ERTS ERGORE!")))

(defun ergore-command-seen (data &rest args)
  "Returns the last time that Emacs has seen jonnay."
  (let ((idle (current-idle-time)))
    (ergore-send (ergore-get-nick data)
                 (format "Jonnay has been away from emacs %s%d.%2d seconds (can you imagine?)"
                         (if (> (first idle) 0)
                             (first idle)
                           "")
                         (second idle)
                         (third idle)))))

(defun ergore-command-ping (data &rest args)
  "Alerts me that you want my attention.  I may not answer right away.  Use poke in an emergency."
  (message "%s %s" 
           (car (erc-parse-user (erc-response.sender data))) 
           (erc-parse-user (erc-response.sender data)))
  (sauron-add-event 'ergore
                    5
                    (concat "MASTER! " (car (erc-parse-user (erc-response.sender data))) " Sent you a ping in ERC.")))

(defun ergore-command-force (data &rest args)
  "FORCE the IM window to the front.  This is the equivalent of yelling at me."
  (let ((cur-buffer (current-buffer)))
    (set-window-point (display-buffer "&bitlbee" '(display-buffer-pop-up-window ((inhibit-same-window . nil))))
                      (save-excursion 
                        (set-buffer "&bitlbee") 
                        (point-max)))))

(defun ergore-command-brain (data &rest args)
  "Show Jonnays CURRENT NEUROLOGICAL EEG STATE. How cool is that?"
  (let ((brain mindwave/current))
    (cond ((or (null (cdr (assoc 'poorSignalLevel brain)))
               (= (cdr (assoc 'poorSignalLevel brain))
                  200))
           (ergore-send (ergore-get-nick data) 
                        "Jonnay Doesn't have his mindwave on."))
          ((> (cdr (assoc 'poorSignalLevel brain))
              50)
           (ergore-send (ergore-get-nick data) 
                        "Jonnay's mindwave has a bad connection right now"))
          (t 
           (ergore-send (ergore-get-nick data) 
                        (list (format "Attentive: %d/100  Relaxed: %d/100"
                                      (mindwave/access-in 'eSense 'attention brain)
                                      (mindwave/access-in 'eSense 'meditation brain))
                              (format "Relative EEG:  δ:%s  θ:%s  α:%s %s  β:%s %s  γ:%s %s "
                                      (mindwave/access-in 'eegPower 'delta brain)                 
                                      (mindwave/access-in 'eegPower 'theta brain)
                                      (mindwave/access-in 'eegPower 'lowAlpha brain)
                                      (mindwave/access-in 'eegPower 'highAlpha brain)
                                      (mindwave/access-in 'eegPower 'lowBeta brain)
                                      (mindwave/access-in 'eegPower 'highBeta brain)
                                      (mindwave/access-in 'eegPower 'lowGamma brain)
                                      (mindwave/access-in 'eegPower 'highGamma brain))))))))

(defun ergore-command-chao (data &rest args)
  "Make me read to you from a chaotic book of wisdom.  Could be long, could be short..."
  (ergore-send (ergore-get-nick data)
               (split-string (emagician/cookie) "\n" t)))

(defun ergore-erc-hook (string)
  "Hooks into ERC and makes ergore go."
  (let ((pos (string-match ergore-invocation string)))
    (when pos (ergore-run-command (substring string (+ pos (length ergore-invocation))))))
  string)

(defun ergore-run-command (cmd-string)
  "Main dispatch for running an ergore command."
  (when ergore-debug
    (message "Ergore received command %s" cmd-string))
  (let* ((cmd-parts (split-string (substring-no-properties cmd-string)))
         (cmd (intern (substring-no-properties (car cmd-parts))))
         (args (cdr cmd-parts))
         (data (get-text-property 0 'erc-parsed cmd-string)))
    (message "CMD: %S  ARGS: %S  DATA:%S" cmd args data)
    (let ((cmd (cdr (assoc cmd ergore-commands))))    
      (if cmd
          (cond ((functionp cmd) 
                 (apply cmd data args))
                ((and (symbolp cmd)
                      (functionp cmd (symbol-value cmd)))
                 (apply (symbol-value cmd) data args))
                (t 
                 (message "Ergore: Someone tried to call %s with %s.  (%s)" cmd args data)))
        (message "Ergore: Unknown command %s" cmd)))))

(add-hook 'erc-insert-pre-hook 'ergore-erc-hook)

(provide 'ergore)
