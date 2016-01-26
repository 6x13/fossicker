;;; fossicker-shader.el --- Fossicker contribs that provides shader type.

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

(fossicker-register-type 'shader nil
                   :regexp '("\\.vert\\'"
                             "\\.frag\\'"
                             "\\.tesc\\'"
                             "\\.tese\\'"
                             "\\.geom\\'"
                             "\\.comp\\'")
                   :formats (lambda (ext)
                              (cond ((member-ignore-case
                                      ext
                                      '("vert" "vrt")) '("vrt" "vert"))
                                    ((member-ignore-case
                                      ext
                                      '("frag" "frg")) '("frag" "frg"))
                                    ((member-ignore-case
                                      ext
                                      '("tese" "tes")) '("tese" "tes"))
                                    (t nil))))

(provide 'fossicker-shader)

;;; fossicker-shader.el ends here
