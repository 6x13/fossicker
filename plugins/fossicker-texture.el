;;; fossicker-texture.el --- Fossicker contribs that provides texture type.

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

(defun fossicker-texture-handler (root context filename ext spec source)
  (when (y-or-n-p (format "Destination: %s%s/%s%s"
                          root
                          (propertize "[SIZE]"
                                      'face
                                      '(:foreground "red"))
                          context
                          filename))
    ;; (make-directory fossickerroot t)
    nil))

(fossicker-register-type 'texture nil
                   :regexp
                   '("\\.png\\'"
                     "\\.jpg\\'"
                     "\\.tiff\\'"
                     "\\.tga\\'")
                   :function 'fossicker-texture-handler
                   :widgets
                   '((repeat :tag "Sizes"
                             :value (("ldpi/" . 32)
                                     ("mdpi/" . 64)
                                     ("hdpi/" . 128))
                             (cons :format "%v"
                                   (directory :size 12
                                              :format "%t: %v\n"
                                              :tag "Path")
                                   (integer :size 4
                                            :format "%t: %v\n"
                                            :tag "Unit Size")))))

(provide 'fossicker-texture)

;;; fossicker-texture.el ends here
