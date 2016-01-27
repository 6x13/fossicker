;;; fossicker-audio.el --- Fossicker contribs that provides audio type.

;; Copyright (C) 2015 Kenan Bölükbaşı

;; Author: Kenan Bölükbaşı <kenanbolukbasi@gmail.com>
;; Created: 17 December 2015
;; Version: 0.1.1
;; Keywords: gamedev, game, development, sprite, fossicker, asset, tools
;; Homepage: http://kenanb.com

;; This file is not part of GNU Emacs.

;; This file is part of Fossicker.

;; Fossicker is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; Fossicker is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Fossicker.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'fossicker)

(fossicker-register-type 'audio nil
                   :regexp '("\\.mp3\\'"
                             "\\.wav\\'"
                             "\\.ogg\\'"
                             "\\.aac\\'")
                   :formats t)

(provide 'fossicker-audio)

;;; fossicker-audio.el ends here