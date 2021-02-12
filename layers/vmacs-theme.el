;;; vmacs-theme.el --- Configure the appearance of Emacs. -*- lexical-binding:t -*-

;;; Commentary:

;;

;;; Code:

(defface dim-parens-face
  '((((class color) (background dark))
     (:foreground "grey50"))
    (((class color) (background light))
     (:foreground "grey55")))
  "Face used to tastefully dim parentheses."
  :group 'basic-faces)

;; Chalk Colours.
;; #8f9ca0 rgb(143, 156, 160) //Ansi 0 Color
;; #c15064 rgb(193, 80, 100) //Ansi 1 Color
;; #90cc82 rgb(144, 204, 130) //Ansi 10 Color
;; #ffc900 rgb(255, 201, 0) //Ansi 11 Color
;; #4ea9ff rgb(78, 169, 255) //Ansi 12 Color
;; #fe6b88 rgb(254, 107, 136) //Ansi 13 Color
;; #60d4c9 rgb(96, 212, 201) //Ansi 14 Color
;; #dadfe0 rgb(218, 223, 224) //Ansi 15 Color
;; #89a97d rgb(137, 169, 125) //Ansi 2 Color
;; #f5d99a rgb(245, 217, 154) //Ansi 3 Color
;; #3391ba rgb(51, 145, 186) //Ansi 4 Color
;; #ca656d rgb(202, 101, 109) //Ansi 5 Color
;; #51b4a9 rgb(81, 180, 169) //Ansi 6 Color
;; #dadfe0 rgb(218, 223, 224) //Ansi 7 Color
;; #999999 rgb(153, 153, 153) //Ansi 8 Color
;; #f76050 rgb(247, 96, 80) //Ansi 9 Color
;; #393a3c rgb(57, 58, 60) //Background Color
;; #ff2600 rgb(255, 38, 0) //Badge Color
;; #eff0ef rgb(239, 240, 239) //Bold Color
;; #839495 rgb(131, 148, 149) //Cursor Color
;; #b3ecff rgb(179, 236, 255) //Cursor Guide Color
;; #003440 rgb(0, 52, 64) //Cursor Text Color
;; #dadfe0 rgb(218, 223, 224) //Foreground Color
;; #005bbb rgb(0, 91, 187) //Link Color
;; #505052 rgb(80, 80, 82) //Selected Text Color
;; #e9edf1 rgb(233, 237, 241) //Selection Color

;; Seoul256 Colours.
;; #000000 rgb(0, 0, 0) //Ansi 0 Color
;; #ba3b38 rgb(186, 59, 56) //Ansi 1 Color
;; #79ab87 rgb(121, 171, 135) //Ansi 10 Color
;; #d1d6b0 rgb(209, 214, 176) //Ansi 11 Color
;; #88afd4 rgb(136, 175, 212) //Ansi 12 Color
;; #e39f89 rgb(227, 159, 137) //Ansi 13 Color
;; #61afab rgb(97, 175, 171) //Ansi 14 Color
;; #d0cfca rgb(208, 207, 202) //Ansi 15 Color
;; #5d865f rgb(93, 134, 95) //Ansi 2 Color
;; #d5a45f rgb(213, 164, 95) //Ansi 3 Color
;; #88afd4 rgb(136, 175, 212) //Ansi 4 Color
;; #cc5a6d rgb(204, 90, 109) //Ansi 5 Color
;; #61afab rgb(97, 175, 171) //Ansi 6 Color
;; #d0cfca rgb(208, 207, 202) //Ansi 7 Color
;; #000000 rgb(0, 0, 0) //Ansi 8 Color
;; #e97073 rgb(233, 112, 115) //Ansi 9 Color
;; #262526 rgb(38, 37, 38) //Background Color <- try this with Chalk
;; #c5c8c6 rgb(197, 200, 198) //Bold Color
;; #c5c8c6 rgb(197, 200, 198) //Cursor Color
;; #1d1f21 rgb(29, 31, 33) //Cursor Text Color
;; #d0d0d0 rgb(208, 208, 208) //Foreground Color
;; #c5c8c6 rgb(197, 200, 198) //Selected Text Color
;; #2a5f5f rgb(42, 95, 95) //Selection Color

(deftheme vmacs-dark
  "Dark theme for vmacs.")

(let ((foreground "")
      (background ""))

  (custom-theme-set-faces
   'vmacs-dark
   '(default ((t (:foreground ,color-1 :background black))))
   '(cursor  ((t (:background ,color-2))))
   '(fringe  ((t (:background ,color-3))))
   )

  (custom-theme-set-variables
   'vmacs-dark
   '(any-variable EXPR)))

(provide 'vmacs-theme)
;;; vmacs-theme.el ends here
