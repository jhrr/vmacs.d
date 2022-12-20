;;; vmacs-labburn-theme.el --- A L*A*B*-ified Zenburn.

;; Original Authors: Bozhidar Batsov
;; Original URL: https://github.com/bbatsov/zenburn-emacs
;; Copyright (C) 2011-2020 Bozhidar Batsov
;; Author: Johannes Goslar
;; URL: https://github.com/ksjogo/labburn-theme
;; Copyright (C) 2015-2020 Johannes Goslar

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Adapted from the Zenburn and Labburn colour themes.

;; https://en.wikipedia.org/wiki/CIELAB_color_space

;;;; Code:

(eval-and-compile
  (deftheme vmacs-labburn "The vmacs-labburn color theme.")

  (defgroup vmacs-faces nil
    "Faces used by vmacs."
    :group 'faces)

  (defface dim-parens-face
    '((t :inherit default))
    "Face used to tastefully dim parentheses."
    :group 'vmacs-faces)

  (defconst vmacs-labburn-base-lightness 80)
  (defconst vmacs-labburn-base-lightness-step 5)
  (defconst vmacs-labburn-base-saturation 25)
  (defconst vmacs-labburn-class '((class color) (min-colors 89)))

  (custom-theme-set-variables
   'vmacs-labburn
   `(rainbow-identifiers-cie-l*a*b*-lightness ,vmacs-labburn-base-lightness t)
   `(rainbow-identifiers-cie-l*a*b*-saturation ,vmacs-labburn-base-saturation t)
   `(rainbow-identifiers-choose-face-function 'rainbow-identifiers-cie-l*a*b*-choose-face t)
   `(rainbow-identifiers-cie-l*a*b*-color-count 1024 t))

  (defun vmacs-labburn-clamp (num low high)
    (min (max num low) high))

  (defconst vmacs-labburn-d65-xyz '(0.950455 1.0 1.088753)
    "D65 white point in CIE XYZ.")

  (defconst vmacs-labburn-cie-ε (/ 216 24389.0))
  (defconst vmacs-labburn-cie-κ (/ 24389 27.0))

  (defun vmacs-labburn-lab-to-xyz (L a b &optional white-point)
    "Convert CIE L*a*b* to CIE XYZ.
WHITE-POINT specifies the (X Y Z) white point for the
conversion.  If omitted or nil, use `vmacs-labburn-d65-xyz'."
    (pcase-let* ((`(,Xr ,Yr ,Zr) (or white-point vmacs-labburn-d65-xyz))
                 (fy (/ (+ L 16) 116.0))
                 (fz (- fy (/ b 200.0)))
                 (fx (+ (/ a 500.0) fy))
                 (xr (if (> (expt fx 3.0) vmacs-labburn-cie-ε)
                         (expt fx 3.0)
                       (/ (- (* fx 116) 16) vmacs-labburn-cie-κ)))
                 (yr (if (> L (* vmacs-labburn-cie-κ vmacs-labburn-cie-ε))
                         (expt (/ (+ L 16) 116.0) 3.0)
                       (/ L vmacs-labburn-cie-κ)))
                 (zr (if (> (expt fz 3) vmacs-labburn-cie-ε)
                         (expt fz 3.0)
                       (/ (- (* 116 fz) 16) vmacs-labburn-cie-κ))))
      (list (* xr Xr)     ; X
            (* yr Yr)     ; Y
            (* zr Zr))))  ; Z

  (defun vmacs-labburn-xyz-to-srgb (X Y Z)
    "Convert CIE X Y Z colors to sRGB color space."
    (let ((r (+ (* 3.2404542 X) (* -1.5371385 Y) (* -0.4985314 Z)))
          (g (+ (* -0.9692660 X) (* 1.8760108 Y) (* 0.0415560 Z)))
          (b (+ (* 0.0556434 X) (* -0.2040259 Y) (* 1.0572252 Z))))
      (list (if (<= r 0.0031308)
                (* 12.92 r)
              (- (* 1.055 (expt r (/ 1 2.4))) 0.055))
            (if (<= g 0.0031308)
                (* 12.92 g)
              (- (* 1.055 (expt g (/ 1 2.4))) 0.055))
            (if (<= b 0.0031308)
                (* 12.92 b)
              (- (* 1.055 (expt b (/ 1 2.4))) 0.055)))))

  (defun vmacs-labburn-rgb-to-hex  (red green blue)
    "Return hexadecimal notation for the color RED GREEN BLUE.
RED, GREEN, and BLUE should be numbers between 0.0 and 1.0, inclusive."
    (format "#%02x%02x%02x"
            (* red 255) (* green 255) (* blue 255)))

  (defun vmacs-labburn-lab-to-hex (L a b)
    (apply 'vmacs-labburn-rgb-to-hex (apply 'vmacs-labburn-xyz-to-srgb (vmacs-labburn-lab-to-xyz L a b))))

  (defun vmacs-labburn-define-color (name a b &optional lightness step)
    (setq lightness (or lightness vmacs-labburn-base-lightness))
    (setq step (or vmacs-labburn-base-lightness-step step))
    (dotimes (i 21)
      (let* ((i (* (- i 10) 0.5))
             (suffix
              (replace-regexp-in-string
               "0$" ""
               (replace-regexp-in-string
                "\\." "" (cond
                          ((< i 0) (number-to-string i))
                          ((> i 0) (concat "+" (number-to-string i)))
                          (t "")))))
             (name (concat name suffix))
             (l (vmacs-labburn-clamp (+ lightness (* i step)) 0 100)))
        (eval `(defconst ,(intern name) (vmacs-labburn-lab-to-hex ,l ,a ,b))))))

  (vmacs-labburn-define-color "vmacs-labburn-red" 21.49605264873783 8.540773333869666 67)
  (vmacs-labburn-define-color "vmacs-labburn-orange" 13.168745714886187 23.101973510068618 75)
  (vmacs-labburn-define-color "vmacs-labburn-yellow" -1.3751424406895363 25.127342438569976 80)
  (vmacs-labburn-define-color "vmacs-labburn-green" -17.534332143205823 13.126938831390866 62 5)
  (vmacs-labburn-define-color "vmacs-labburn-blue" -20.560356403992287 -8.311105653507678 80)
  (vmacs-labburn-define-color "vmacs-labburn-magenta" 38.335076954957806 -15.842566128814228 67)
  (vmacs-labburn-define-color "vmacs-labburn-cyan" -22.87217931855784 -8.997815401280684 84)
  ;; TODO: vmacs-bg ->  (vmacs-labburn-define-color "vmacs-labburn-bg" 24.38 -0.09 -1.40)
  (vmacs-labburn-define-color "vmacs-labburn-bg" 0 0 27 5)
  (vmacs-labburn-define-color "vmacs-labburn-fg" -2.7768240550932743 7.856188033624156 87)

  (defconst vmacs-bg "#393a3c")
  (defconst vmacs-black "#242424")
  (defconst vmacs-bold "#eff0ef")
  (defconst vmacs-error "#f76050")
  (defconst vmacs-golden "#ffc900")
  (defconst vmacs-grey "#242424")
  (defconst vmacs-mode-line "#bfbfbf")
  (defconst vmacs-dim-parens "#7f7f7f")
  (defconst vmacs-red "#c15064")
  (defconst vmacs-string "#cd9494")
  (defconst vmacs-comments "#89a97d")
  (defconst vmacs-link "#88afd4")
  (defconst vmacs-keyword "#f5d99a")

  (defvar vmacs-labburn-highlight "yellow"))

(custom-theme-set-faces
 'vmacs-labburn

 ;; --- custom faces
 `(dim-parens-face ((t (:foreground ,vmacs-dim-parens))))

 ;; --- basic
 `(border ((t (:background ,vmacs-bg :foreground ,vmacs-labburn-fg))))
 `(border-color ((t (:background ,vmacs-bg))))
 `(button ((t (:foreground ,vmacs-labburn-yellow :underline t))))
 `(cursor ((t (:foreground ,vmacs-labburn-fg :background ,vmacs-labburn-fg+1))))
 `(default ((t (:foreground ,vmacs-labburn-fg :background ,vmacs-bg))))
 `(error ((t (:foreground ,vmacs-error))))
 `(escape-glyph ((t (:foreground ,vmacs-labburn-yellow :weight bold))))
 `(header-line ((t (:foreground ,vmacs-labburn-yellow :background ,vmacs-bg :extend t))))
 `(highlight ((t (:background ,vmacs-labburn-bg+1))))
 `(horizontal-border ((t (:background ,vmacs-bg))))
 `(link ((t (:foreground ,vmacs-labburn-yellow :underline t))))
 `(link-visited ((t (:foreground ,vmacs-labburn-yellow-2 :underline t :weight normal))))
 `(menu ((t (:foreground ,vmacs-labburn-fg :background ,vmacs-bg))))
 `(minibuffer-prompt ((t (:foreground ,vmacs-labburn-yellow))))
 `(region ((,vmacs-labburn-class (:background ,vmacs-labburn-bg-1 :extend t)) (t :inverse-video t)))
 `(secondary-selection ((t (:background ,vmacs-labburn-bg+2))))
 `(success ((t (:foreground ,vmacs-labburn-green))))
 `(tooltip ((t (:foreground ,vmacs-labburn-fg :background ,vmacs-labburn-bg+1))))
 `(trailing-whitespace ((t (:background ,vmacs-labburn-red))))
 ;; `(variable-pitch ((t (:family "DejaVu Sans"))))
 `(vertical-border ((t (:foreground ,vmacs-mode-line))))
 `(warning ((t (:foreground ,vmacs-labburn-orange))))
 `(widget-field ((t (:foreground ,vmacs-labburn-fg :background ,vmacs-labburn-bg+3))))

 ;; --- font lock
 `(font-lock-builtin-face ((t (:foreground ,vmacs-labburn-fg+1))))
 `(font-lock-comment-face ((t (:foreground ,vmacs-labburn-green))))
 `(font-lock-comment-delimiter-face ((t (:foreground ,vmacs-labburn-green-2))))
 `(font-lock-constant-face ((t (:foreground ,vmacs-labburn-green+4))))
 `(font-lock-doc-face ((t (:foreground ,vmacs-labburn-green+2))))
 `(font-lock-function-name-face ((t (:foreground ,vmacs-labburn-cyan))))
 `(font-lock-keyword-face ((t (:foreground ,vmacs-labburn-yellow))))
 `(font-lock-negation-char-face ((t (:foreground ,vmacs-labburn-yellow))))
 `(font-lock-preprocessor-face ((t (:foreground ,vmacs-labburn-blue+1))))
 `(font-lock-regexp-grouping-construct ((t (:foreground ,vmacs-labburn-yellow))))
 `(font-lock-regexp-grouping-backslash ((t (:foreground ,vmacs-labburn-green))))
 `(font-lock-string-face ((t (:foreground ,vmacs-labburn-red))))
 `(font-lock-type-face ((t (:foreground ,vmacs-labburn-blue-1))))
 `(font-lock-variable-name-face ((t (:foreground ,vmacs-labburn-fg))))
 `(font-lock-warning-face ((t (:foreground ,vmacs-labburn-yellow-2))))
 `(c-annotation-face ((t (:inherit font-lock-constant-face))))

 ;; --- mode-line
 `(mode-line ((t (:foreground ,vmacs-black :background ,vmacs-mode-line :box nil))))
 `(mode-line-buffer-id ((t (:foreground ,vmacs-black))))
 `(show-paren-mismatch ((t (:foreground ,vmacs-labburn-red))))
 `(show-paren-match ((t (:foreground ,vmacs-bold))))

 ;; --- compilation
 `(compilation-column-face ((t (:foreground ,vmacs-labburn-yellow))))
 `(compilation-enter-directory-face ((t (:foreground ,vmacs-labburn-green))))
 `(compilation-error-face ((t (:foreground ,vmacs-labburn-red-1 :underline t))))
 `(compilation-error ((t (:foreground ,vmacs-error))))
 `(compilation-face ((t (:foreground ,vmacs-labburn-fg))))
 `(compilation-info-face ((t (:foreground ,vmacs-labburn-blue))))
 `(compilation-info ((t (:foreground ,vmacs-labburn-green+4 :underline t))))
 `(compilation-leave-directory-face ((t (:foreground ,vmacs-labburn-green))))
 `(compilation-line-face ((t (:foreground ,vmacs-labburn-yellow))))
 `(compilation-line-number ((t (:foreground ,vmacs-labburn-yellow))))
 `(compilation-message-face ((t (:foreground ,vmacs-labburn-blue))))
 `(compilation-warning-face ((t (:foreground ,vmacs-labburn-orange :underline t))))
 `(compilation-mode-line-exit ((t (:foreground ,vmacs-labburn-green+2))))
 `(compilation-mode-line-fail ((t (:foreground ,vmacs-labburn-red))))
 `(compilation-mode-line-run ((t (:foreground ,vmacs-labburn-yellow))))

 ;; --- custom
 `(custom-face-tag ((t (:foreground ,vmacs-labburn-blue))))
 `(custom-group-tag ((t (:foreground ,vmacs-labburn-blue))))
 `(custom-link ((t (:foreground ,vmacs-labburn-yellow :underline t))))
 `(custom-variable-tag ((t (:foreground ,vmacs-labburn-blue))))
 `(custom-state ((t (:foreground ,vmacs-labburn-green))))

 ;; --- completions
 `(completions-annotations ((t (:foreground ,vmacs-labburn-fg-1))))

 ;; --- display-fill-column-indicator
 `(fill-column-indicator ((,vmacs-labburn-class :foreground ,vmacs-labburn-bg-05 :weight semilight)))

 ;; --- eww
 '(eww-invalid-certificate ((t (:inherit error))))
 '(eww-valid-certificate   ((t (:inherit success))))

 ;; --- grep
 `(grep-context-face ((t (:foreground ,vmacs-labburn-fg))))
 `(grep-error-face ((t (:foreground ,vmacs-labburn-red-1 :underline t))))
 `(grep-hit-face ((t (:foreground ,vmacs-labburn-blue))))
 `(grep-match-face ((t (:foreground ,vmacs-labburn-orange))))
 `(match ((t (:background ,vmacs-labburn-bg-1 :foreground ,vmacs-labburn-orange))))

 ;; --- hi-lock
 `(hi-blue    ((t (:background ,vmacs-labburn-cyan    :foreground ,vmacs-labburn-bg-1))))
 `(hi-green   ((t (:background ,vmacs-labburn-green+4 :foreground ,vmacs-labburn-bg-1))))
 `(hi-pink    ((t (:background ,vmacs-labburn-magenta :foreground ,vmacs-labburn-bg-1))))
 `(hi-yellow  ((t (:background ,vmacs-labburn-yellow  :foreground ,vmacs-labburn-bg-1))))
 `(hi-blue-b  ((t (:foreground ,vmacs-labburn-blue))))
 `(hi-green-b ((t (:foreground ,vmacs-labburn-green+2))))
 `(hi-red-b   ((t (:foreground ,vmacs-labburn-red))))

 ;; --- info
 `(Info-quoted ((t (:inherit font-lock-constant-face))))

 ;; --- isearch
 `(isearch ((t (:foreground ,vmacs-labburn-highlight))))
 `(isearch-fail ((t (:foreground ,vmacs-labburn-fg :background ,vmacs-labburn-red-4))))
 `(lazy-highlight ((t (:foreground ,vmacs-labburn-yellow-2 :background ,vmacs-labburn-bg-05))))

 ;; --- man
 '(Man-overstrike ((t (:inherit font-lock-keyword-face))))
 '(Man-underline  ((t (:inherit (font-lock-string-face underline)))))

 ;; --- woman
 '(woman-bold   ((t (:inherit font-lock-keyword-face))))
 '(woman-italic ((t (:inherit (font-lock-string-face italic)))))

 ;;; Third-party
 ;; --- ace-jump
 `(ace-jump-face-background
   ((t (:inverse-video nil))))
 `(ace-jump-face-foreground
   ((t (:foreground ,vmacs-labburn-highlight))))

 ;; --- ace-window
 `(aw-background-face
   ((t (:foreground ,vmacs-labburn-fg-1 :background ,vmacs-bg :inverse-video nil))))
 `(aw-leading-char-face ((t (:inherit aw-mode-line-face))))

 ;; --- auto-complete
 `(ac-candidate-face ((t (:background ,vmacs-labburn-bg+3 :foreground ,vmacs-labburn-bg-2))))
 `(ac-selection-face ((t (:background ,vmacs-labburn-blue-4 :foreground ,vmacs-labburn-fg))))
 `(popup-tip-face ((t (:background ,vmacs-labburn-yellow-2 :foreground ,vmacs-labburn-bg-2))))
 `(popup-menu-mouse-face ((t (:background ,vmacs-labburn-yellow-2 :foreground ,vmacs-labburn-bg-2))))
 `(popup-summary-face ((t (:background ,vmacs-labburn-bg+3 :foreground ,vmacs-labburn-bg-2))))
 `(popup-scroll-bar-foreground-face ((t (:background ,vmacs-labburn-blue-5))))
 `(popup-scroll-bar-background-face ((t (:background ,vmacs-labburn-bg-1))))
 `(popup-isearch-match ((t (:background ,vmacs-bg :foreground ,vmacs-labburn-fg))))

 ;; --- avy
 `(avy-background-face
   ((t (:foreground ,vmacs-labburn-fg-3 :background ,vmacs-bg :inverse-video nil))))
 `(avy-lead-face-0
   ((t (:foreground ,vmacs-labburn-highlight :background ,vmacs-bg :inverse-video nil))))
 `(avy-lead-face-1
   ((t (:foreground ,vmacs-labburn-highlight :background ,vmacs-bg :inverse-video nil))))
 `(avy-lead-face-2
   ((t (:foreground ,vmacs-labburn-highlight :background ,vmacs-bg :inverse-video nil))))
 `(avy-lead-face
   ((t (:foreground ,vmacs-labburn-highlight :background ,vmacs-bg :inverse-video nil))))

 ;; --- auctex
 `(font-latex-bold-face ((t (:inherit bold))))
 `(font-latex-warning-face ((t (:foreground nil :inherit font-lock-warning-face))))
 `(font-latex-sectioning-5-face ((t (:foreground ,vmacs-labburn-red ))))
 `(font-latex-sedate-face ((t (:foreground ,vmacs-labburn-yellow))))
 `(font-latex-italic-face ((t (:foreground ,vmacs-labburn-fg :slant italic))))
 `(font-latex-string-face ((t (:inherit ,font-lock-string-face))))
 `(font-latex-math-face ((t (:foreground ,vmacs-labburn-orange))))
 `(font-latex-script-char-face ((t (:foreground ,vmacs-labburn-orange))))
 `(TeX-fold-folded-face ((t (:foreground ,vmacs-labburn-orange))))
 `(TeX-fold-unfolded-face ((t (:background ,vmacs-labburn-bg+2))))

 ;; --- magic-latex
 `(ml/subsection ((t (:foreground ,vmacs-labburn-red :height 1.2))))
 `(ml/section ((t (:foreground ,vmacs-labburn-red :height 1.6))))
 `(ml/chapter ((t (:foreground ,vmacs-labburn-red :height 1.8))))
 `(ml/title ((t (:foreground ,vmacs-labburn-red :height 2.0))))
 `(jg/tex-header ((t (:foreground ,vmacs-labburn-red))))

 ;; --- bm
 `(bm-face ((t (:background ,vmacs-labburn-yellow-1 :foreground ,vmacs-bg))))
 `(bm-fringe-face ((t (:background ,vmacs-labburn-yellow-1 :foreground ,vmacs-bg))))
 `(bm-fringe-persistent-face ((t (:background ,vmacs-labburn-green-2 :foreground ,vmacs-bg))))
 `(bm-persistent-face ((t (:background ,vmacs-labburn-green-2 :foreground ,vmacs-bg))))

 ;; --- centaur-tabs
 `(centaur-tabs-default ((t (:background ,vmacs-bg :foreground ,vmacs-labburn-fg :box nil))))
 `(centaur-tabs-selected ((t (:background ,vmacs-bg :foreground ,vmacs-labburn-fg+2 :box nil))))
 `(centaur-tabs-unselected ((t (:background ,vmacs-labburn-bg-1 :foreground ,vmacs-labburn-fg-05 :box nil))))
 `(centaur-tabs-selected-modified ((t (:background ,vmacs-bg :foreground ,vmacs-labburn-orange :box nil))))
 `(centaur-tabs-unselected-modified ((t (:background ,vmacs-labburn-bg-1 :foreground ,vmacs-labburn-orange :box nil))))
 `(centaur-tabs-active-bar-face ((t (:background ,vmacs-labburn-yellow :box nil))))
 `(centaur-tabs-modified-marker-selected ((t (:inherit 'centaur-tabs-selected-modified :foreground ,vmacs-labburn-yellow :box nil))))
 `(centaur-tabs-modified-marker-unselected ((t (:inherit 'centaur-tabs-unselected-modified :foreground ,vmacs-labburn-yellow :box nil))))

 ;; --- calfw
 `(cfw:face-annotation ((t (:foreground ,vmacs-labburn-red :inherit cfw:face-day-title))))
 `(cfw:face-day-title ((t nil)))
 `(cfw:face-default-content ((t (:foreground ,vmacs-labburn-green))))
 `(cfw:face-default-day ((t (:weight bold))))
 `(cfw:face-disable ((t (:foreground ,vmacs-labburn-fg-1))))
 `(cfw:face-grid ((t (:inherit shadow))))
 `(cfw:face-header ((t (:inherit font-lock-keyword-face))))
 `(cfw:face-holiday ((t (:inherit cfw:face-sunday))))
 `(cfw:face-periods ((t (:foreground ,vmacs-labburn-cyan))))
 `(cfw:face-saturday ((t (:foreground ,vmacs-labburn-blue :weight bold))))
 `(cfw:face-select ((t (:background ,vmacs-labburn-blue-5))))
 `(cfw:face-sunday ((t (:foreground ,vmacs-labburn-red :weight bold))))
 `(cfw:face-title ((t (:height 2.0 :inherit (variable-pitch font-lock-keyword-face)))))
 `(cfw:face-today ((t (:foreground ,vmacs-labburn-cyan :weight bold))))
 `(cfw:face-today-title ((t (:inherit highlight bold))))
 `(cfw:face-toolbar ((t (:background ,vmacs-labburn-blue-5))))
 `(cfw:face-toolbar-button-off ((t (:underline nil :inherit link))))
 `(cfw:face-toolbar-button-on ((t (:underline nil :inherit link-visited))))

 ;; --- cider
 `(cider-result-overlay-face ((t (:background unspecified))))
 `(cider-enlightened-face ((t (:box (:color ,vmacs-labburn-orange :line-width -1)))))
 `(cider-enlightened-local-face ((t (:weight bold :foreground ,vmacs-labburn-green+1))))
 `(cider-deprecated-face ((t (:background ,vmacs-labburn-yellow-2))))
 `(cider-instrumented-face ((t (:box (:color ,vmacs-labburn-red :line-width -1)))))
 `(cider-traced-face ((t (:box (:color ,vmacs-labburn-cyan :line-width -1)))))
 `(cider-test-failure-face ((t (:background ,vmacs-labburn-red-4))))
 `(cider-test-error-face ((t (:background ,vmacs-labburn-magenta))))
 `(cider-test-success-face ((t (:background ,vmacs-labburn-green-2))))
 `(cider-fringe-good-face ((t (:foreground ,vmacs-labburn-green+4))))

 ;; --- company-mode
 `(company-tooltip ((t (:foreground ,vmacs-labburn-fg :background ,vmacs-labburn-bg+1 :weight normal))))
 `(company-tooltip-selection ((t (:background ,vmacs-labburn-bg+2 :weight normal))))
 `(company-tooltip-mouse ((t (:background ,vmacs-labburn-bg-1 :weight normal))))
 `(company-tooltip-common ((t (:foreground ,vmacs-labburn-orange :background ,vmacs-labburn-bg+1 :weight normal))))
 `(company-tooltip-annotation-selection ((t (:foreground ,vmacs-labburn-orange :background ,vmacs-labburn-bg-1))))
 `(company-tooltip-annotation ((t (:foreground "#999999" :background ,vmacs-labburn-bg+1 :weight normal))))
 `(company-tooltip-common-selection ((t (:foreground ,vmacs-labburn-orange :background ,vmacs-labburn-bg+1 :weight normal))))
 `(company-scrollbar-fg ((t (:background ,vmacs-labburn-orange))))
 `(company-scrollbar-bg ((t (:background ,vmacs-labburn-bg+2))))
 `(company-preview ((t (:background ,vmacs-labburn-bg-1))))
 `(company-preview-common ((t (:foreground ,vmacs-labburn-fg))))
 `(company-preview-search ((t (:foreground ,vmacs-labburn-yellow))))
 `(company-template-field ((t (:background ,vmacs-labburn-bg-1))))

 ;; --- company-quickhelp
 `(company-quickhelp-color-background ,vmacs-labburn-bg+1)
 `(company-quickhelp-color-foreground ,vmacs-labburn-fg)

 ;; --- context-coloring
 `(context-coloring-level-0-face ((t :foreground ,vmacs-labburn-fg)))
 `(context-coloring-level-1-face ((t :foreground ,vmacs-labburn-cyan)))
 `(context-coloring-level-2-face ((t :foreground ,vmacs-labburn-green+4)))
 `(context-coloring-level-3-face ((t :foreground ,vmacs-labburn-yellow)))
 `(context-coloring-level-4-face ((t :foreground ,vmacs-labburn-orange)))
 `(context-coloring-level-5-face ((t :foreground ,vmacs-labburn-magenta)))
 `(context-coloring-level-6-face ((t :foreground ,vmacs-labburn-blue+1)))
 `(context-coloring-level-7-face ((t :foreground ,vmacs-labburn-green+2)))
 `(context-coloring-level-8-face ((t :foreground ,vmacs-labburn-yellow-2)))
 `(context-coloring-level-9-face ((t :foreground ,vmacs-labburn-red+1)))

 ;; --- coq
 `(coq-solve-tactics-face ((t (:foreground nil :inherit font-lock-constant-face))))
 ;; --- ctable
 `(ctbl:face-cell-select ((t (:background ,vmacs-labburn-blue :foreground ,vmacs-bg))))
 `(ctbl:face-continue-bar ((t (:background ,vmacs-labburn-bg-05 :foreground ,vmacs-bg))))
 `(ctbl:face-row-select ((t (:background ,vmacs-labburn-cyan :foreground ,vmacs-bg))))

 ;; --- debbugs
 `(debbugs-gnu-done ((t (:foreground ,vmacs-labburn-fg-1))))
 `(debbugs-gnu-handled ((t (:foreground ,vmacs-labburn-green))))
 `(debbugs-gnu-new ((t (:foreground ,vmacs-labburn-red))))
 `(debbugs-gnu-pending ((t (:foreground ,vmacs-labburn-blue))))
 `(debbugs-gnu-stale ((t (:foreground ,vmacs-labburn-orange))))
 `(debbugs-gnu-tagged ((t (:foreground ,vmacs-labburn-red))))

 ;; --- diff
 `(diff-added          ((t (:background "#335533" :foreground ,vmacs-labburn-green))))
 `(diff-changed        ((t (:background "#555511" :foreground ,vmacs-labburn-yellow-1))))
 `(diff-removed        ((t (:background "#553333" :foreground ,vmacs-labburn-red-2))))
 `(diff-refine-added   ((t (:background "#338833" :foreground ,vmacs-labburn-green+4))))
 `(diff-refine-changed ((t (:background "#888811" :foreground ,vmacs-labburn-yellow))))
 `(diff-refine-removed ((t (:background "#883333" :foreground ,vmacs-labburn-red))))
 `(diff-header ((,vmacs-labburn-class (:background ,vmacs-labburn-bg+2))
                (t (:background ,vmacs-labburn-fg :foreground ,vmacs-bg))))
 `(diff-file-header
   ((,vmacs-labburn-class (:background ,vmacs-labburn-bg+2 :foreground ,vmacs-labburn-fg :weight bold))
    (t (:background ,vmacs-labburn-fg :foreground ,vmacs-bg :weight bold))))

 ;; --- diff-hl
 `(diff-hl-change ((,vmacs-labburn-class (:foreground ,vmacs-labburn-blue :background ,vmacs-labburn-blue-2))))
 `(diff-hl-delete ((,vmacs-labburn-class (:foreground ,vmacs-labburn-red+1 :background ,vmacs-labburn-red-1))))
 `(diff-hl-insert ((,vmacs-labburn-class (:foreground ,vmacs-labburn-green+1 :background ,vmacs-labburn-green-2))))

 ;; --- dired
 `(dired-header ((t (:foreground ,vmacs-labburn-orange))))
 `(dired-directory ((t (:foreground ,vmacs-labburn-orange))))
 `(dired-marked ((t (:foreground ,vmacs-labburn-highlight))))
 `(dired-mark ((t (:foreground ,vmacs-labburn-highlight))))
 `(dired-perm-write ((t (:foreground ,vmacs-labburn-red+2))))

 ;; --- dired+
 `(diredp-display-msg ((t (:foreground ,vmacs-labburn-blue))))
 `(diredp-compressed-file-suffix ((t (:foreground ,vmacs-labburn-orange))))
 `(diredp-date-time ((t (:foreground ,vmacs-labburn-fg-2))))
 `(diredp-deletion ((t (:foreground ,vmacs-labburn-yellow))))
 `(diredp-deletion-file-name ((t (:foreground ,vmacs-labburn-red))))
 `(diredp-dir-heading ((t (:foreground ,vmacs-labburn-orange))))
 `(diredp-dir-name ((t (:foreground ,vmacs-labburn-orange))))
 `(diredp-dir-priv ((t (:foreground ,vmacs-labburn-orange-2))))
 `(diredp-exec-priv ((t (:foreground ,vmacs-labburn-red-2))))
 `(diredp-executable-tag ((t (:foreground ,vmacs-labburn-green+1))))
 `(diredp-file-name ((t (:foreground ,vmacs-labburn-fg))))
 `(diredp-file-suffix ((t (:foreground ,vmacs-labburn-fg))))
 `(diredp-flag-mark ((t (:foreground ,vmacs-labburn-yellow))))
 `(diredp-flag-mark-line ((t (:foreground ,vmacs-labburn-orange))))
 `(diredp-ignored-file-name ((t (:foreground ,vmacs-labburn-fg-4))))
 `(diredp-link-priv ((t (:foreground ,vmacs-labburn-yellow-2))))
 `(diredp-mode-line-flagged ((t (:foreground ,vmacs-labburn-yellow))))
 `(diredp-mode-line-marked ((t (:foreground ,vmacs-labburn-orange))))
 `(diredp-no-priv ((t (:foreground ,vmacs-labburn-fg-2))))
 `(diredp-number ((t (:foreground ,vmacs-labburn-green-2))))
 `(diredp-other-priv ((t (:foreground ,vmacs-labburn-yellow-2))))
 `(diredp-rare-priv ((t (:foreground ,vmacs-labburn-red-2))))
 `(diredp-read-priv ((t (:foreground ,vmacs-labburn-green-2))))
 `(diredp-symlink ((t (:foreground ,vmacs-labburn-yellow))))
 `(diredp-write-priv ((t (:foreground ,vmacs-labburn-magenta-2))))

 ;; --- diredfl
 `(diredfl-compressed-file-suffix ((t (:foreground ,vmacs-labburn-orange))))
 `(diredfl-date-time ((t (:foreground ,vmacs-labburn-magenta))))
 `(diredfl-deletion ((t (:foreground ,vmacs-labburn-yellow))))
 `(diredfl-deletion-file-name ((t (:foreground ,vmacs-labburn-red))))
 `(diredfl-dir-heading ((t (:foreground ,vmacs-labburn-blue :background ,vmacs-labburn-bg-1))))
 `(diredfl-dir-priv ((t (:foreground ,vmacs-labburn-cyan))))
 `(diredfl-exec-priv ((t (:foreground ,vmacs-labburn-red))))
 `(diredfl-executable-tag ((t (:foreground ,vmacs-labburn-green+1))))
 `(diredfl-file-name ((t (:foreground ,vmacs-labburn-blue))))
 `(diredfl-file-suffix ((t (:foreground ,vmacs-labburn-green))))
 `(diredfl-flag-mark ((t (:foreground ,vmacs-labburn-yellow))))
 `(diredfl-flag-mark-line ((t (:foreground ,vmacs-labburn-orange))))
 `(diredfl-ignored-file-name ((t (:foreground ,vmacs-labburn-red))))
 `(diredfl-link-priv ((t (:foreground ,vmacs-labburn-yellow))))
 `(diredfl-no-priv ((t (:foreground ,vmacs-labburn-fg))))
 `(diredfl-number ((t (:foreground ,vmacs-labburn-green+1))))
 `(diredfl-other-priv ((t (:foreground ,vmacs-labburn-yellow-1))))
 `(diredfl-rare-priv ((t (:foreground ,vmacs-labburn-red-1))))
 `(diredfl-read-priv ((t (:foreground ,vmacs-labburn-green-2))))
 `(diredfl-symlink ((t (:foreground ,vmacs-labburn-yellow))))
 `(diredfl-write-priv ((t (:foreground ,vmacs-labburn-magenta))))

 ;; --- dired-async
 `(dired-async-failures ((t (:foreground ,vmacs-labburn-red))))
 `(dired-async-message ((t (:foreground ,vmacs-labburn-yellow))))
 `(dired-async-mode-message ((t (:foreground ,vmacs-labburn-yellow))))

 ;; --- edebug
 `(hi-edebug-x-debug-line ((t (:foreground ,vmacs-labburn-highlight))))
 `(hi-edebug-x-stop ((t (:background ,vmacs-labburn-blue-5))))

 ;; --- ediff
 `(ediff-current-diff-A ((t (:foreground ,vmacs-labburn-fg :background ,vmacs-labburn-red-4))))
 `(ediff-current-diff-Ancestor ((t (:foreground ,vmacs-labburn-fg :background ,vmacs-labburn-red-4))))
 `(ediff-current-diff-B ((t (:foreground ,vmacs-labburn-fg :background ,vmacs-labburn-green-2))))
 `(ediff-current-diff-C ((t (:foreground ,vmacs-labburn-fg :background ,vmacs-labburn-blue-5))))
 `(ediff-even-diff-A ((t (:background ,vmacs-labburn-bg+1))))
 `(ediff-even-diff-Ancestor ((t (:background ,vmacs-labburn-bg+1))))
 `(ediff-even-diff-B ((t (:background ,vmacs-labburn-bg+1))))
 `(ediff-even-diff-C ((t (:background ,vmacs-labburn-bg+1))))
 `(ediff-fine-diff-A ((t (:foreground ,vmacs-labburn-fg :background ,vmacs-labburn-red-2 :weight bold))))
 `(ediff-fine-diff-Ancestor ((t (:foreground ,vmacs-labburn-fg :background ,vmacs-labburn-red-2 weight bold))))
 `(ediff-fine-diff-B ((t (:foreground ,vmacs-labburn-fg :background ,vmacs-labburn-green :weight bold))))
 `(ediff-fine-diff-C ((t (:foreground ,vmacs-labburn-fg :background ,vmacs-labburn-blue-3 :weight bold ))))
 `(ediff-odd-diff-A ((t (:background ,vmacs-labburn-bg+2))))
 `(ediff-odd-diff-Ancestor ((t (:background ,vmacs-labburn-bg+2))))
 `(ediff-odd-diff-B ((t (:background ,vmacs-labburn-bg+2))))
 `(ediff-odd-diff-C ((t (:background ,vmacs-labburn-bg+2))))

 ;; --- elfeed
 `(elfeed-log-error-level-face ((t (:foreground ,vmacs-labburn-red))))
 `(elfeed-log-info-level-face ((t (:foreground ,vmacs-labburn-blue))))
 `(elfeed-log-warn-level-face ((t (:foreground ,vmacs-labburn-yellow))))
 `(elfeed-search-date-face ((t (:foreground ,vmacs-labburn-yellow-1 :underline t))))
 `(elfeed-search-tag-face ((t (:foreground ,vmacs-labburn-green))))
 `(elfeed-search-feed-face ((t (:foreground ,vmacs-labburn-cyan))))
 `(elfeed-search-unread-title-face ((t (:foreground ,vmacs-labburn-fg+2))))
 `(elfeed-search-title-face ((t (:foreground ,vmacs-labburn-fg-1))))

 ;; --- erc
 `(erc-action-face ((t (:inherit erc-default-face))))
 `(erc-bold-face ((t (:weight bold))))
 `(erc-current-nick-face ((t (:foreground ,vmacs-labburn-blue))))
 `(erc-dangerous-host-face ((t (:inherit font-lock-warning-face))))
 `(erc-default-face ((t (:foreground ,vmacs-labburn-fg))))
 `(erc-direct-msg-face ((t (:inherit erc-default-face))))
 `(erc-error-face ((t (:inherit font-lock-warning-face))))
 `(erc-fool-face ((t (:inherit erc-default-face))))
 `(erc-highlight-face ((t (:inherit hover-highlight))))
 `(erc-input-face ((t (:foreground ,vmacs-labburn-yellow))))
 `(erc-keyword-face ((t (:foreground ,vmacs-labburn-blue))))
 `(erc-nick-default-face ((t (:foreground ,vmacs-labburn-yellow))))
 `(erc-my-nick-face ((t (:foreground ,vmacs-labburn-red))))
 `(erc-nick-msg-face ((t (:inherit erc-default-face))))
 `(erc-notice-face ((t (:foreground ,vmacs-labburn-green))))
 `(erc-pal-face ((t (:foreground ,vmacs-labburn-orange))))
 `(erc-prompt-face ((t (:foreground ,vmacs-labburn-orange :background ,vmacs-bg))))
 `(erc-timestamp-face ((t (:foreground ,vmacs-labburn-green+4))))
 `(erc-underline-face ((t (:underline t))))

 ;; --- eros
 `(eros-result-overlay-face ((t (:background unspecified))))

 ;; --- eshell
 `(eshell-prompt ((t (:foreground ,vmacs-labburn-yellow))))
 `(eshell-ls-archive ((t (:foreground ,vmacs-labburn-red-1))))
 `(eshell-ls-backup ((t (:inherit font-lock-comment-face))))
 `(eshell-ls-clutter ((t (:inherit font-lock-comment-face))))
 `(eshell-ls-directory ((t (:foreground ,vmacs-labburn-blue+1))))
 `(eshell-ls-executable ((t (:foreground ,vmacs-labburn-red+1))))
 `(eshell-ls-unreadable ((t (:foreground ,vmacs-labburn-fg))))
 `(eshell-ls-missing ((t (:inherit font-lock-warning-face))))
 `(eshell-ls-product ((t (:inherit font-lock-doc-face))))
 `(eshell-ls-special ((t (:foreground ,vmacs-labburn-yellow))))
 `(eshell-ls-symlink ((t (:foreground ,vmacs-labburn-cyan))))

 ;; --- eval-sexp-fu-flash
 `(eval-sexp-fu-flash ((t (:foreground ,vmacs-labburn-highlight))))
 `(eval-sexp-fu-flash-error ((t (:foreground "red"))))

 ;; --- flx
 `(flx-highlight-face ((t (:foreground ,vmacs-labburn-green+2))))

 ;; --- flycheck
 `(flycheck-error
   ((((supports :underline (:style wave)))
     (:underline (:style wave :color ,vmacs-labburn-red-1) :inherit unspecified))
    (t (:foreground ,vmacs-labburn-red-1 :underline t))))
 `(flycheck-warning
   ((((supports :underline (:style wave)))
     (:underline (:style wave :color ,vmacs-labburn-yellow) :inherit unspecified))
    (t (:foreground ,vmacs-labburn-yellow :underline t))))
 `(flycheck-info
   ((((supports :underline (:style wave)))
     (:underline (:style wave :color ,vmacs-labburn-cyan) :inherit unspecified))
    (t (:foreground ,vmacs-labburn-cyan :underline t))))
 `(flycheck-fringe-error ((t (:foreground ,vmacs-labburn-red-1))))
 `(flycheck-fringe-warning ((t (:foreground ,vmacs-labburn-yellow))))
 `(flycheck-fringe-info ((t (:foreground ,vmacs-labburn-cyan))))

 ;; --- flyspell
 `(flyspell-duplicate
   ((((supports :underline (:style wave)))
     (:underline (:style wave :color ,vmacs-labburn-orange) :inherit unspecified))
    (t (:foreground ,vmacs-labburn-orange :underline t))))
 `(flyspell-incorrect
   ((((supports :underline (:style wave)))
     (:underline (:style wave :color ,vmacs-labburn-red) :inherit unspecified))
    (t (:foreground ,vmacs-labburn-red-1 :underline t))))

 ;; --- geben
 `(geben-backtrace-fileuri ((t (:foreground ,vmacs-labburn-green+1))))

 ;; --- git-annex
 '(git-annex-dired-annexed-available ((t (:inherit success :weight normal))))
 '(git-annex-dired-annexed-unavailable ((t (:inherit error :weight normal))))
 ;; --- git-commit
 `(git-commit-comment-action ((,vmacs-labburn-class (:foreground ,vmacs-labburn-green+1))))
 `(git-commit-comment-branch  ((,vmacs-labburn-class (:foreground ,vmacs-labburn-blue+1)))) ; obsolete
 `(git-commit-comment-branch-local  ((,vmacs-labburn-class (:foreground ,vmacs-labburn-blue+1))))
 `(git-commit-comment-branch-remote ((,vmacs-labburn-class (:foreground ,vmacs-labburn-green))))
 `(git-commit-summary ((,vmacs-labburn-class (:foreground ,vmacs-labburn-orange))))
 `(git-commit-comment-heading ((,vmacs-labburn-class (:foreground ,vmacs-labburn-yellow))))

 ;; --- git-rebase
 `(git-rebase-hash ((t (:foreground ,vmacs-labburn-orange))))

 ;; --- gnus
 `(gnus-group-mail-1 ((t (:weight bold :inherit gnus-group-mail-1-empty))))
 `(gnus-group-mail-1-empty ((t (:inherit gnus-group-news-1-empty))))
 `(gnus-group-mail-2 ((t (:weight bold :inherit gnus-group-mail-2-empty))))
 `(gnus-group-mail-2-empty ((t (:inherit gnus-group-news-2-empty))))
 `(gnus-group-mail-3 ((t (:weight bold :inherit gnus-group-mail-3-empty))))
 `(gnus-group-mail-3-empty ((t (:inherit gnus-group-news-3-empty))))
 `(gnus-group-mail-4 ((t (:weight bold :inherit gnus-group-mail-4-empty))))
 `(gnus-group-mail-4-empty ((t (:inherit gnus-group-news-4-empty))))
 `(gnus-group-mail-5 ((t (:weight bold :inherit gnus-group-mail-5-empty))))
 `(gnus-group-mail-5-empty ((t (:inherit gnus-group-news-5-empty))))
 `(gnus-group-mail-6 ((t (:weight bold :inherit gnus-group-mail-6-empty))))
 `(gnus-group-mail-6-empty ((t (:inherit gnus-group-news-6-empty))))
 `(gnus-group-mail-low ((t (:weight bold :inherit gnus-group-mail-low-empty))))
 `(gnus-group-mail-low-empty ((t (:inherit gnus-group-news-low-empty))))
 `(gnus-group-news-1 ((t (:weight bold :inherit gnus-group-news-1-empty))))
 `(gnus-group-news-2 ((t (:weight bold :inherit gnus-group-news-2-empty))))
 `(gnus-group-news-3 ((t (:weight bold :inherit gnus-group-news-3-empty))))
 `(gnus-group-news-4 ((t (:weight bold :inherit gnus-group-news-4-empty))))
 `(gnus-group-news-5 ((t (:weight bold :inherit gnus-group-news-5-empty))))
 `(gnus-group-news-6 ((t (:weight bold :inherit gnus-group-news-6-empty))))
 `(gnus-group-news-low ((t (:weight bold :inherit gnus-group-news-low-empty))))
 `(gnus-header-content ((t (:inherit message-header-other))))
 `(gnus-header-from ((t (:inherit message-header-to))))
 `(gnus-header-name ((t (:inherit message-header-name))))
 `(gnus-header-newsgroups ((t (:inherit message-header-other))))
 `(gnus-header-subject ((t (:inherit message-header-subject))))
 `(gnus-server-opened ((t (:foreground ,vmacs-labburn-green+2))))
 `(gnus-server-denied ((t (:foreground ,vmacs-labburn-red+1))))
 `(gnus-server-closed ((t (:foreground ,vmacs-labburn-blue :slant italic))))
 `(gnus-server-offline ((t (:foreground ,vmacs-labburn-yellow))))
 `(gnus-server-agent ((t (:foreground ,vmacs-labburn-blue))))
 `(gnus-summary-cancelled ((t (:foreground ,vmacs-labburn-orange))))
 `(gnus-summary-high-ancient ((t (:foreground ,vmacs-labburn-blue))))
 `(gnus-summary-high-read ((t (:foreground ,vmacs-labburn-green))))
 `(gnus-summary-high-ticked ((t (:foreground ,vmacs-labburn-orange))))
 `(gnus-summary-high-unread ((t (:foreground ,vmacs-labburn-fg))))
 `(gnus-summary-low-ancient ((t (:foreground ,vmacs-labburn-blue))))
 `(gnus-summary-low-read ((t (:foreground ,vmacs-labburn-green))))
 `(gnus-summary-low-ticked ((t (:foreground ,vmacs-labburn-orange))))
 `(gnus-summary-low-unread ((t (:foreground ,vmacs-labburn-fg))))
 `(gnus-summary-normal-ancient ((t (:foreground ,vmacs-labburn-blue))))
 `(gnus-summary-normal-read ((t (:foreground ,vmacs-labburn-green))))
 `(gnus-summary-normal-ticked ((t (:foreground ,vmacs-labburn-orange))))
 `(gnus-summary-normal-unread ((t (:foreground ,vmacs-labburn-fg))))
 `(gnus-summary-selected ((t (:foreground ,vmacs-labburn-yellow))))
 `(gnus-cite-1 ((t (:foreground ,vmacs-labburn-blue))))
 `(gnus-cite-10 ((t (:foreground ,vmacs-labburn-yellow-1))))
 `(gnus-cite-11 ((t (:foreground ,vmacs-labburn-yellow))))
 `(gnus-cite-2 ((t (:foreground ,vmacs-labburn-blue-1))))
 `(gnus-cite-3 ((t (:foreground ,vmacs-labburn-blue-2))))
 `(gnus-cite-4 ((t (:foreground ,vmacs-labburn-green+2))))
 `(gnus-cite-5 ((t (:foreground ,vmacs-labburn-green+1))))
 `(gnus-cite-6 ((t (:foreground ,vmacs-labburn-green))))
 `(gnus-cite-7 ((t (:foreground ,vmacs-labburn-red))))
 `(gnus-cite-8 ((t (:foreground ,vmacs-labburn-red-1))))
 `(gnus-cite-9 ((t (:foreground ,vmacs-labburn-red-2))))
 `(gnus-group-news-1-empty ((t (:foreground ,vmacs-labburn-yellow))))
 `(gnus-group-news-2-empty ((t (:foreground ,vmacs-labburn-green+3))))
 `(gnus-group-news-3-empty ((t (:foreground ,vmacs-labburn-green+1))))
 `(gnus-group-news-4-empty ((t (:foreground ,vmacs-labburn-blue-2))))
 `(gnus-group-news-5-empty ((t (:foreground ,vmacs-labburn-blue-3))))
 `(gnus-group-news-6-empty ((t (:foreground ,vmacs-labburn-bg+2))))
 `(gnus-group-news-low-empty ((t (:foreground ,vmacs-labburn-bg+2))))
 `(gnus-signature ((t (:foreground ,vmacs-labburn-yellow))))
 `(gnus-x ((t (:background ,vmacs-labburn-fg :foreground ,vmacs-bg))))
 `(mm-uu-extract ((t (:background ,vmacs-labburn-bg-05 :foreground ,vmacs-labburn-green+1))))

 ;; --- go-guru
 `(go-guru-hl-identifier-face ((t (:foreground ,vmacs-labburn-bg-1 :background ,vmacs-labburn-green+1))))

 ;; --- guide-key
 `(guide-key/highlight-command-face ((t (:foreground ,vmacs-labburn-blue))))
 `(guide-key/key-face ((t (:foreground ,vmacs-labburn-green))))
 `(guide-key/prefix-command-face ((t (:foreground ,vmacs-labburn-green+1))))

 ;; --- helm
 `(helm-header
   ((t (:foreground ,vmacs-labburn-green :background ,vmacs-bg :underline nil :box nil :extend t))))
 `(helm-selection ((t (:background ,vmacs-labburn-bg+1 :underline nil))))
 `(helm-selection-line ((t (:background ,vmacs-labburn-bg+1))))
 `(helm-visible-mark ((t (:foreground ,vmacs-bg :background ,vmacs-labburn-yellow-2))))
 `(helm-candidate-number ((t (:foreground ,vmacs-labburn-green+4 :background ,vmacs-labburn-bg-1))))
 `(helm-separator ((t (:foreground ,vmacs-labburn-red :background ,vmacs-bg))))
 `(helm-time-zone-current ((t (:foreground ,vmacs-labburn-green+2 :background ,vmacs-bg))))
 `(helm-time-zone-home ((t (:foreground ,vmacs-labburn-red :background ,vmacs-bg))))
 `(helm-bookmark-addressbook ((t (:foreground ,vmacs-labburn-orange :background ,vmacs-bg))))
 `(helm-bookmark-directory ((t (:foreground nil :background nil :inherit helm-ff-directory))))
 `(helm-bookmark-file ((t (:foreground nil :background nil :inherit helm-ff-file))))
 `(helm-bookmark-gnus ((t (:foreground ,vmacs-labburn-magenta :background ,vmacs-bg))))
 `(helm-bookmark-info ((t (:foreground ,vmacs-labburn-green+2 :background ,vmacs-bg))))
 `(helm-bookmark-man ((t (:foreground ,vmacs-labburn-yellow :background ,vmacs-bg))))
 `(helm-bookmark-w3m ((t (:foreground ,vmacs-labburn-magenta :background ,vmacs-bg))))
 `(helm-buffer-not-saved ((t (:foreground ,vmacs-labburn-red :background ,vmacs-bg))))
 `(helm-buffer-process ((t (:foreground ,vmacs-labburn-cyan :background ,vmacs-bg))))
 `(helm-buffer-saved-out ((t (:foreground ,vmacs-labburn-fg :background ,vmacs-bg))))
 `(helm-buffer-size ((t (:foreground ,vmacs-labburn-fg-1 :background ,vmacs-bg))))
 `(helm-ff-directory ((t (:foreground ,vmacs-labburn-orange :background ,vmacs-bg))))
 `(helm-ff-file ((t (:foreground ,vmacs-labburn-fg :background ,vmacs-bg :weight normal))))
 `(helm-ff-executable ((t (:foreground ,vmacs-labburn-green+2 :background ,vmacs-bg :weight normal))))
 `(helm-ff-invalid-symlink ((t (:foreground ,vmacs-labburn-red :background ,vmacs-bg))))
 `(helm-ff-symlink ((t (:foreground ,vmacs-labburn-yellow :background ,vmacs-bg))))
 `(helm-ff-prefix ((t (:foreground ,vmacs-bg :background ,vmacs-labburn-yellow :weight normal))))
 `(helm-grep-cmd-line ((t (:foreground ,vmacs-labburn-cyan :background ,vmacs-bg))))
 `(helm-grep-file ((t (:foreground ,vmacs-labburn-fg :background ,vmacs-bg))))
 `(helm-grep-finish ((t (:foreground ,vmacs-labburn-green+2 :background ,vmacs-bg))))
 `(helm-grep-lineno ((t (:foreground ,vmacs-labburn-yellow :background ,vmacs-bg))))
 `(helm-grep-match ((t (:foreground nil :background nil :inherit helm-match))))
 `(helm-grep-running ((t (:foreground ,vmacs-labburn-red :background ,vmacs-bg))))
 `(helm-match ((t (:foreground ,vmacs-labburn-yellow :background ,vmacs-bg))))
 `(helm-moccur-buffer ((t (:foreground ,vmacs-labburn-green+4 :background ,vmacs-bg))))
 `(helm-mu-contacts-address-face ((t (:foreground ,vmacs-labburn-fg-1 :background ,vmacs-bg))))
 `(helm-mu-contacts-name-face ((t (:foreground ,vmacs-labburn-fg :background ,vmacs-bg))))
 `(helm-source-header ((t (:foreground ,vmacs-labburn-yellow :background ,vmacs-labburn-bg-2 :underline nil :extend t))))
 `(helm-swoop-target-line-block-face ((t (:background ,vmacs-labburn-bg+1))))
 `(helm-swoop-target-line-face ((t (:background ,vmacs-labburn-bg+1))))
 `(helm-swoop-target-word-face ((t (:foreground ,vmacs-labburn-highlight))))

 ;; --- helm-lxc
 `(helm-lxc-face-frozen ((t (:foreground ,vmacs-labburn-blue :background ,vmacs-bg))))
 `(helm-lxc-face-running ((t (:foreground ,vmacs-labburn-green :background ,vmacs-bg))))
 `(helm-lxc-face-stopped ((t (:foreground ,vmacs-labburn-red :background ,vmacs-bg))))

 ;; --- highlight-symbol
 `(highlight-symbol-face ((t (:background ,vmacs-labburn-bg+2))))

 ;; --- highlight-thing
 `(highlight-thing ((t (:background ,vmacs-labburn-bg+2))))

 ;; --- hl-line-mode
 `(hl-line-face ((,vmacs-labburn-class (:background ,vmacs-labburn-bg-05))
                 (t :weight bold)))
 `(hl-line ((,vmacs-labburn-class (:background ,vmacs-labburn-bg-05 :extend t)) ; old emacsen
            (t :weight bold)))

 ;; --- hydra
 `(hydra-face-red ((t (:foreground ,vmacs-labburn-red-1 :background ,vmacs-bg))))
 `(hydra-face-amaranth ((t (:foreground ,vmacs-labburn-red-3 :background ,vmacs-bg))))
 `(hydra-face-blue ((t (:foreground ,vmacs-labburn-blue :background ,vmacs-bg))))
 `(hydra-face-pink ((t (:foreground ,vmacs-labburn-magenta :background ,vmacs-bg))))
 `(hydra-face-teal ((t (:foreground ,vmacs-labburn-cyan :background ,vmacs-bg))))

 ;; --- info+
 `(info-command-ref-item ((t (:background ,vmacs-labburn-bg-1 :foreground ,vmacs-labburn-orange))))
 `(info-constant-ref-item ((t (:background ,vmacs-labburn-bg-1 :foreground ,vmacs-labburn-magenta))))
 `(info-double-quoted-name ((t (:inherit font-lock-comment-face))))
 `(info-file ((t (:background ,vmacs-labburn-bg-1 :foreground ,vmacs-labburn-yellow))))
 `(info-function-ref-item ((t (:background ,vmacs-labburn-bg-1 :inherit font-lock-function-name-face))))
 `(info-macro-ref-item ((t (:background ,vmacs-labburn-bg-1 :foreground ,vmacs-labburn-yellow))))
 `(info-menu ((t (:foreground ,vmacs-labburn-yellow))))
 `(info-quoted-name ((t (:inherit font-lock-constant-face))))
 `(info-reference-item ((t (:background ,vmacs-labburn-bg-1))))
 `(info-single-quote ((t (:inherit font-lock-keyword-face))))
 `(info-special-form-ref-item ((t (:background ,vmacs-labburn-bg-1 :foreground ,vmacs-labburn-yellow))))
 `(info-string ((t (:inherit font-lock-string-face))))
 `(info-syntax-class-item ((t (:background ,vmacs-labburn-bg-1 :foreground ,vmacs-labburn-blue+1))))
 `(info-user-option-ref-item ((t (:background ,vmacs-labburn-bg-1 :foreground ,vmacs-labburn-red))))
 `(info-variable-ref-item ((t (:background ,vmacs-labburn-bg-1 :foreground ,vmacs-labburn-orange))))

 ;; --- iedit-mode
 `(iedit-occurrence ((t (:background ,vmacs-labburn-bg+2))))

 ;; --- irfc
 `(irfc-head-name-face ((t (:foreground ,vmacs-labburn-red))))
 `(irfc-head-number-face ((t (:foreground ,vmacs-labburn-red))))
 `(irfc-reference-face ((t (:foreground ,vmacs-labburn-blue-1))))
 `(irfc-requirement-keyword-face ((t (:inherit font-lock-keyword-face))))
 `(irfc-rfc-link-face ((t (:inherit link))))
 `(irfc-rfc-number-face ((t (:foreground ,vmacs-labburn-cyan))))
 `(irfc-std-number-face ((t (:foreground ,vmacs-labburn-green+4))))
 `(irfc-table-item-face ((t (:foreground ,vmacs-labburn-green+3))))
 `(irfc-title-face ((t (:foreground ,vmacs-labburn-yellow :underline t))))

 ;; --- ivy
 `(ivy-confirm-face ((t (:foreground ,vmacs-labburn-green :background ,vmacs-bg))))
 `(ivy-current-match ((t (:background ,vmacs-labburn-bg+1))))
 `(ivy-cursor ((t (:foreground ,vmacs-bg :background ,vmacs-labburn-fg))))
 `(ivy-match-required-face ((t (:foreground ,vmacs-labburn-red :background ,vmacs-bg))))
 `(ivy-minibuffer-match-face-1 ((t (:background ,vmacs-labburn-bg+1))))
 `(ivy-minibuffer-match-face-2 ((t (:background ,vmacs-labburn-bg+1))))
 `(ivy-minibuffer-match-face-3 ((t (:background ,vmacs-labburn-bg+1))))
 `(ivy-minibuffer-match-face-4 ((t (:background ,vmacs-labburn-bg+1))))
 `(ivy-remote ((t (:foreground ,vmacs-labburn-blue :background ,vmacs-bg))))
 `(ivy-subdir ((t (:foreground ,vmacs-labburn-yellow :background ,vmacs-bg))))

 ;; --- js2-mode
 `(js2-warning ((t (:underline (:style wave :color ,vmacs-labburn-orange)))))
 `(js2-error ((t (:underline (:style wave :color ,vmacs-labburn-red)))))
 `(js2-jsdoc-tag ((t (:inherit font-lock-doc-face))))
 `(js2-jsdoc-type ((t (:inherit font-lock-doc-face))))
 `(js2-jsdoc-value ((t (:inherit font-lock-doc-face))))
 `(js2-function-param ((t (:foreground ,vmacs-labburn-orange))))
 `(js2-external-variable ((t (:underline (:style wave :color ,vmacs-labburn-orange)))))
 `(js2-jsdoc-html-tag-delimiter ((t (:inherit web-mode-html-tag-face))))
 `(js2-jsdoc-html-tag-name ((t (:inherit web-mode-html-tag-face))))

 ;; --- additional js2 mode attributes for better syntax highlighting
 `(js2-instance-member ((t (:foreground ,vmacs-labburn-green-2))))
 `(js2-jsdoc-html-tag-delimiter ((t (:foreground ,vmacs-labburn-orange))))
 `(js2-jsdoc-html-tag-name ((t (:foreground ,vmacs-labburn-red-1))))
 `(js2-object-property ((t (:foreground ,vmacs-labburn-blue+1))))
 `(js2-magic-paren ((t (:foreground ,vmacs-labburn-blue-5))))
 `(js2-private-function-call ((t (:foreground ,vmacs-labburn-cyan))))
 `(js2-function-call ((t (:foreground ,vmacs-labburn-cyan))))
 `(js2-private-member ((t (:foreground ,vmacs-labburn-blue-1))))
 `(js2-keywords ((t (:foreground ,vmacs-labburn-magenta))))

 ;; --- ledger-mode
 `(ledger-font-payee-uncleared-face ((t (:foreground ,vmacs-labburn-red-1))))
 `(ledger-font-payee-cleared-face ((t (:foreground ,vmacs-labburn-fg))))
 `(ledger-font-payee-pending-face ((t (:foreground ,vmacs-labburn-red))))
 `(ledger-font-xact-highlight-face ((t (:background ,vmacs-labburn-bg+1))))
 `(ledger-font-auto-xact-face ((t (:foreground ,vmacs-labburn-yellow-1))))
 `(ledger-font-periodic-xact-face ((t (:foreground ,vmacs-labburn-green))))
 `(ledger-font-pending-face ((t (:foreground ,vmacs-labburn-orange))))
 `(ledger-font-other-face ((t (:foreground ,vmacs-labburn-fg))))
 `(ledger-font-posting-date-face ((t (:foreground ,vmacs-labburn-orange))))
 `(ledger-font-posting-account-face ((t (:foreground ,vmacs-labburn-blue-1))))
 `(ledger-font-posting-account-cleared-face ((t (:foreground ,vmacs-labburn-fg))))
 `(ledger-font-posting-account-pending-face ((t (:foreground ,vmacs-labburn-orange))))
 `(ledger-font-posting-amount-face ((t (:foreground ,vmacs-labburn-orange))))
 `(ledger-occur-narrowed-face ((t (:foreground ,vmacs-labburn-fg-1 :invisible t))))
 `(ledger-occur-xact-face ((t (:background ,vmacs-labburn-bg+1))))
 `(ledger-font-comment-face ((t (:foreground ,vmacs-labburn-green))))
 `(ledger-font-reconciler-uncleared-face ((t (:foreground ,vmacs-labburn-red-1))))
 `(ledger-font-reconciler-cleared-face ((t (:foreground ,vmacs-labburn-fg))))
 `(ledger-font-reconciler-pending-face ((t (:foreground ,vmacs-labburn-orange))))
 `(ledger-font-report-clickable-face ((t (:foreground ,vmacs-labburn-orange))))

 ;; --- linum-mode
 `(linum ((t (:foreground ,vmacs-labburn-bg+3 :background ,vmacs-bg))))
 `(linum-relative-current-face ((t (:inherit linum))))

 ;; --- lispy
 `(lispy-command-name-face ((t (:background ,vmacs-labburn-bg-05 :inherit font-lock-function-name-face))))
 `(lispy-cursor-face ((t (:foreground ,vmacs-bg :background ,vmacs-labburn-fg))))
 `(lispy-face-hint ((t (:inherit highlight :foreground ,vmacs-labburn-yellow))))

 ;; --- ruler-mode
 `(ruler-mode-column-number ((t (:inherit 'ruler-mode-default :foreground ,vmacs-labburn-fg))))
 `(ruler-mode-fill-column ((t (:inherit 'ruler-mode-default :foreground ,vmacs-labburn-yellow))))
 `(ruler-mode-goal-column ((t (:inherit 'ruler-mode-fill-column))))
 `(ruler-mode-comment-column ((t (:inherit 'ruler-mode-fill-column))))
 `(ruler-mode-tab-stop ((t (:inherit 'ruler-mode-fill-column))))
 `(ruler-mode-current-column ((t (:foreground ,vmacs-labburn-yellow :box t))))
 `(ruler-mode-default ((t (:foreground ,vmacs-labburn-green+2 :background ,vmacs-bg))))

 ;; --- magit
 `(magit-section-highlight ((t (:background ,vmacs-labburn-bg+05))))
 `(magit-section-heading ((t (:foreground ,vmacs-labburn-yellow))))
 `(magit-section-heading-selection ((t (:foreground ,vmacs-labburn-orange))))
 `(magit-diff-file-heading ((t (:weight bold))))
 `(magit-diff-file-heading-highlight ((t (:background ,vmacs-labburn-bg+05))))
 `(magit-diff-file-heading-selection ((t (:background ,vmacs-labburn-bg+05 :foreground ,vmacs-labburn-orange))))
 `(magit-diff-added ((t (:background ,vmacs-labburn-green-2))))
 `(magit-diff-added-highlight ((t (:background ,vmacs-labburn-green))))
 `(magit-diff-removed ((t (:background ,vmacs-labburn-red-4))))
 `(magit-diff-removed-highlight ((t (:background ,vmacs-labburn-red-3))))
 `(magit-diff-hunk-heading ((t (:background ,vmacs-labburn-bg+1))))
 `(magit-diff-hunk-heading-highlight ((t (:background ,vmacs-labburn-bg+2))))
 `(magit-diff-hunk-heading-selection ((t (:background ,vmacs-labburn-bg+2 :foreground ,vmacs-labburn-orange))))
 `(magit-diff-lines-heading ((t (:background ,vmacs-labburn-orange :foreground ,vmacs-labburn-bg+2))))
 `(magit-diff-context-highlight ((t (:background ,vmacs-labburn-bg+05 :foreground "grey70"))))
 `(magit-diffstat-added ((t (:foreground ,vmacs-labburn-green+4))))
 `(magit-diffstat-removed ((t (:foreground ,vmacs-labburn-red))))

 `(magit-popup-heading ((t (:foreground ,vmacs-labburn-yellow ))))
 `(magit-popup-key ((t (:foreground ,vmacs-labburn-green-2))))
 `(magit-popup-argument ((t (:foreground ,vmacs-labburn-green  ))))
 `(magit-popup-disabled-argument ((t (:foreground ,vmacs-labburn-fg-1    :weight normal))))
 `(magit-popup-option-value ((t (:foreground ,vmacs-labburn-blue-2 ))))

 `(magit-process-ok ((t (:foreground ,vmacs-labburn-green ))))
 `(magit-process-ng ((t (:foreground ,vmacs-labburn-red   ))))

 `(magit-log-author ((t (:foreground ,vmacs-labburn-orange))))
 `(magit-log-date ((t (:foreground ,vmacs-labburn-fg-1))))
 `(magit-log-graph ((t (:foreground ,vmacs-labburn-fg+1))))

 `(magit-sequence-pick ((t (:foreground ,vmacs-labburn-yellow-2))))
 `(magit-sequence-stop ((t (:foreground ,vmacs-labburn-green))))
 `(magit-sequence-part ((t (:foreground ,vmacs-labburn-yellow))))
 `(magit-sequence-head ((t (:foreground ,vmacs-labburn-blue))))
 `(magit-sequence-drop ((t (:foreground ,vmacs-labburn-red))))
 `(magit-sequence-done ((t (:foreground ,vmacs-labburn-fg-1))))
 `(magit-sequence-onto ((t (:foreground ,vmacs-labburn-fg-1))))

 `(magit-bisect-good ((t (:foreground ,vmacs-labburn-green))))
 `(magit-bisect-skip ((t (:foreground ,vmacs-labburn-yellow))))
 `(magit-bisect-bad  ((t (:foreground ,vmacs-labburn-red))))

 `(magit-blame-heading ((t (:background ,vmacs-labburn-bg-1 :foreground ,vmacs-labburn-blue-2))))
 `(magit-blame-hash ((t (:background ,vmacs-labburn-bg-1 :foreground ,vmacs-labburn-blue-2))))
 `(magit-blame-name ((t (:background ,vmacs-labburn-bg-1 :foreground ,vmacs-labburn-orange))))
 `(magit-blame-date ((t (:background ,vmacs-labburn-bg-1 :foreground ,vmacs-labburn-orange))))
 `(magit-blame-summary ((t (:background ,vmacs-labburn-bg-1 :foreground ,vmacs-labburn-blue-2
                                        ))))
 `(magit-dimmed ((t (:foreground ,vmacs-labburn-bg+3))))
 `(magit-hash ((t (:foreground ,vmacs-labburn-bg+3))))
 `(magit-tag ((t (:foreground ,vmacs-labburn-orange))))
 `(magit-branch-remote ((t (:foreground ,vmacs-labburn-green ))))
 `(magit-branch-local ((t (:foreground ,vmacs-labburn-blue  ))))
 `(magit-branch-current ((t (:foreground ,vmacs-labburn-blue   :box t))))
 `(magit-head ((t (:foreground ,vmacs-labburn-blue  ))))
 `(magit-refname ((t (:background ,vmacs-labburn-bg+2 :foreground ,vmacs-labburn-fg))))
 `(magit-refname-stash ((t (:background ,vmacs-labburn-bg+2 :foreground ,vmacs-labburn-fg))))
 `(magit-refname-wip ((t (:background ,vmacs-labburn-bg+2 :foreground ,vmacs-labburn-fg))))
 `(magit-signature-good ((t (:foreground ,vmacs-labburn-green))))
 `(magit-signature-bad ((t (:foreground ,vmacs-labburn-red))))
 `(magit-signature-untrusted ((t (:foreground ,vmacs-labburn-yellow))))
 `(magit-cherry-unmatched ((t (:foreground ,vmacs-labburn-cyan))))
 `(magit-cherry-equivalent ((t (:foreground ,vmacs-labburn-magenta))))
 `(magit-reflog-commit ((t (:foreground ,vmacs-labburn-green))))
 `(magit-reflog-amend ((t (:foreground ,vmacs-labburn-magenta))))
 `(magit-reflog-merge ((t (:foreground ,vmacs-labburn-green))))
 `(magit-reflog-checkout ((t (:foreground ,vmacs-labburn-blue))))
 `(magit-reflog-reset ((t (:foreground ,vmacs-labburn-red))))
 `(magit-reflog-rebase ((t (:foreground ,vmacs-labburn-magenta))))
 `(magit-reflog-cherry-pick ((t (:foreground ,vmacs-labburn-green))))
 `(magit-reflog-remote ((t (:foreground ,vmacs-labburn-cyan))))
 `(magit-reflog-other ((t (:foreground ,vmacs-labburn-cyan))))

 ;; --- markup-faces
 `(markup-anchor-face ((t (:foreground ,vmacs-labburn-blue+1))))
 `(markup-code-face ((t (:inherit font-lock-constant-face))))
 `(markup-command-face ((t (:foreground ,vmacs-labburn-yellow))))
 `(markup-emphasis-face ((t (:inherit bold))))
 `(markup-internal-reference-face ((t (:foreground ,vmacs-labburn-yellow-2 :underline t))))
 `(markup-list-face ((t (:foreground ,vmacs-labburn-fg+1))))
 `(markup-meta-face ((t (:foreground ,vmacs-labburn-yellow))))
 `(markup-meta-hide-face ((t (:foreground ,vmacs-labburn-yellow))))
 `(markup-secondary-text-face ((t (:foreground ,vmacs-labburn-yellow-1))))
 `(markup-title-0-face ((t (:inherit font-lock-function-name-face))))
 `(markup-title-1-face ((t (:inherit font-lock-function-name-face))))
 `(markup-title-2-face ((t (:inherit font-lock-function-name-face))))
 `(markup-title-3-face ((t (:inherit font-lock-function-name-face))))
 `(markup-title-4-face ((t (:inherit font-lock-function-name-face))))
 `(markup-typewriter-face ((t (:inherit font-lock-constant-face))))
 `(markup-verbatim-face ((t (:inherit font-lock-constant-face))))
 `(markup-value-face ((t (:foreground ,vmacs-labburn-yellow))))

 ;; --- message-mode
 `(message-cited-text ((t (:inherit font-lock-comment-face))))
 `(message-header-name ((t (:foreground ,vmacs-labburn-green+1))))
 `(message-header-other ((t (:foreground ,vmacs-labburn-green))))
 `(message-header-to ((t (:foreground ,vmacs-labburn-yellow))))
 `(message-header-cc ((t (:foreground ,vmacs-labburn-yellow))))
 `(message-header-newsgroups ((t (:foreground ,vmacs-labburn-yellow))))
 `(message-header-subject ((t (:foreground ,vmacs-labburn-orange))))
 `(message-header-xheader ((t (:foreground ,vmacs-labburn-green))))
 `(message-mml ((t (:foreground ,vmacs-labburn-yellow))))
 `(message-separator ((t (:inherit font-lock-comment-face))))

 ;; --- mingus
 `(mingus-directory-face ((t (:foreground ,vmacs-labburn-blue))))
 `(mingus-pausing-face ((t (:foreground ,vmacs-labburn-magenta))))
 `(mingus-playing-face ((t (:foreground ,vmacs-labburn-cyan))))
 `(mingus-playlist-face ((t (:foreground ,vmacs-labburn-cyan ))))
 `(mingus-mark-face ((t (:bold t :foreground ,vmacs-labburn-magenta))))
 `(mingus-song-file-face ((t (:foreground ,vmacs-labburn-yellow))))
 `(mingus-artist-face ((t (:foreground ,vmacs-labburn-cyan))))
 `(mingus-album-face ((t (:underline t :foreground ,vmacs-labburn-red+1))))
 `(mingus-album-stale-face ((t (:foreground ,vmacs-labburn-red+1))))
 `(mingus-stopped-face ((t (:foreground ,vmacs-labburn-red))))

 ;; --- mini-header-line
 `(mini-header-line-active ((t (:background ,vmacs-labburn-bg-2))))

 ;; --- merlin
 `(merlin-type-face ((t (:inherit highlight))))
 `(merlin-compilation-warning-face
   ((((supports :underline (:style wave)))
     (:underline (:style wave :color ,vmacs-labburn-orange)))
    (t
     (:underline ,vmacs-labburn-orange))))
 `(merlin-compilation-error-face
   ((((supports :underline (:style wave)))
     (:underline (:style wave :color ,vmacs-labburn-red)))
    (t
     (:underline ,vmacs-labburn-red))))

 ;; --- mu4e
 `(mu4e-header-key-face ((t (:foreground ,vmacs-labburn-yellow))))
 `(mu4e-header-value-face ((t (:foreground ,vmacs-labburn-orange))))

 ;; --- org-mode
 `(org-agenda-date-today ((t (:foreground ,vmacs-labburn-fg+1 :slant italic))) t)
 `(org-agenda-structure ((t (:inherit font-lock-comment-face))))
 `(org-archived ((t (:foreground ,vmacs-labburn-fg))))
 `(org-block ((t (:background ,vmacs-labburn-bg+05 :extend t))))
 `(org-checkbox ((t (:background ,vmacs-labburn-bg+2 :foreground ,vmacs-labburn-fg+1 :box (:line-width 1 :style released-button)))))
 `(org-date ((t (:foreground ,vmacs-labburn-blue :underline t))))
 `(org-deadline-announce ((t (:foreground ,vmacs-labburn-red-1))))
 `(org-done ((t (:weight bold :foreground ,vmacs-labburn-green+3))))
 `(org-formula ((t (:foreground ,vmacs-labburn-yellow-2))))
 `(org-headline-done ((t (:foreground ,vmacs-labburn-green+3))))
 `(org-hide ((t (:foreground ,vmacs-labburn-bg-1))))
 `(org-level-1 ((t (:foreground ,vmacs-labburn-orange))))
 `(org-level-2 ((t (:foreground ,vmacs-labburn-green+4))))
 `(org-level-3 ((t (:foreground ,vmacs-labburn-blue-1))))
 `(org-level-4 ((t (:foreground ,vmacs-labburn-yellow-2))))
 `(org-level-5 ((t (:foreground ,vmacs-labburn-cyan))))
 `(org-level-6 ((t (:foreground ,vmacs-labburn-green+2))))
 `(org-level-7 ((t (:foreground ,vmacs-labburn-red-4))))
 `(org-level-8 ((t (:foreground ,vmacs-labburn-blue-4))))
 `(org-link ((t (:foreground ,vmacs-labburn-yellow-2 :underline t))))
 `(org-quote ((t (:background ,vmacs-labburn-bg+05 :extend t))))
 `(org-scheduled ((t (:foreground ,vmacs-labburn-green+4))))
 `(org-scheduled-previously ((t (:foreground ,vmacs-labburn-red))))
 `(org-scheduled-today ((t (:foreground ,vmacs-labburn-blue+1))))
 `(org-sexp-date ((t (:foreground ,vmacs-labburn-blue+1 :underline t))))
 `(org-special-keyword ((t (:inherit font-lock-comment-face))))
 `(org-table ((t (:foreground ,vmacs-labburn-green+2))))
 `(org-tag ((t (:foreground ,vmacs-labburn-fg :weight bold))))
 `(org-time-grid ((t (:foreground ,vmacs-labburn-orange))))
 `(org-todo ((t (:weight bold :foreground ,vmacs-labburn-red))))
 `(org-upcoming-deadline ((t (:inherit font-lock-keyword-face))))
 `(org-warning ((t (:weight bold :foreground ,vmacs-labburn-red :underline nil))))
 `(org-column ((t (:background ,vmacs-labburn-bg-1))))
 `(org-column-title ((t (:background ,vmacs-labburn-bg-1 :underline t))))
 `(org-mode-line-clock ((t (:foreground ,vmacs-labburn-fg :background ,vmacs-labburn-bg-1))))
 `(org-mode-line-clock-overrun ((t (:foreground ,vmacs-bg :background ,vmacs-labburn-red-1))))
 `(org-ellipsis ((t (:foreground ,vmacs-labburn-yellow-1))))
 `(org-footnote ((t (:foreground ,vmacs-labburn-cyan :underline t))))
 `(org-document-title ((t (:foreground ,vmacs-labburn-blue))))
 `(org-document-info ((t (:foreground ,vmacs-labburn-blue))))
 `(org-habit-ready-face ((t :background ,vmacs-labburn-green)))
 `(org-habit-alert-face ((t :background ,vmacs-labburn-yellow-1 :foreground ,vmacs-bg)))
 `(org-habit-clear-face ((t :background ,vmacs-labburn-blue-3)))
 `(org-habit-overdue-face ((t :background ,vmacs-labburn-red-3)))
 `(org-habit-clear-future-face ((t :background ,vmacs-labburn-blue-4)))
 `(org-habit-ready-future-face ((t :background ,vmacs-labburn-green-2)))
 `(org-habit-alert-future-face ((t :background ,vmacs-labburn-yellow-2 :foreground ,vmacs-bg)))
 `(org-habit-overdue-future-face ((t :background ,vmacs-labburn-red-4)))

 ;; --- org-ref
 `(org-ref-ref-face ((t :underline t)))
 `(org-ref-label-face ((t :underline t)))
 `(org-ref-cite-face ((t :underline t)))
 `(org-ref-glossary-face ((t :underline t)))
 `(org-ref-acronym-face ((t :underline t)))

 ;; --- outline
 `(outline-1 ((t (:foreground ,vmacs-labburn-orange))))
 `(outline-2 ((t (:foreground ,vmacs-labburn-green+4))))
 `(outline-3 ((t (:foreground ,vmacs-labburn-blue-1))))
 `(outline-4 ((t (:foreground ,vmacs-labburn-yellow-2))))
 `(outline-5 ((t (:foreground ,vmacs-labburn-cyan))))
 `(outline-6 ((t (:foreground ,vmacs-labburn-green+2))))
 `(outline-7 ((t (:foreground ,vmacs-labburn-red-4))))
 `(outline-8 ((t (:foreground ,vmacs-labburn-blue-4))))

 ;; --- c/perl
 `(cperl-nonoverridable-face ((t (:foreground ,vmacs-labburn-magenta))))
 `(cperl-array-face ((t (:foreground ,vmacs-labburn-yellow, :backgorund ,vmacs-bg))))
 `(cperl-hash-face ((t (:foreground ,vmacs-labburn-yellow-1, :background ,vmacs-bg))))

 ;; --- paren-face
 `(parenthesis ((t (:foreground ,vmacs-labburn-fg-1))))

 ;; --- pdf-tools
 `(pdf-view-midnight-colors '(,vmacs-labburn-fg . ,vmacs-labburn-bg-05))

 ;; --- perspective
 `(persp-selected-face ((t (:foreground ,vmacs-labburn-yellow-2))))

 ;; --- rainbow-delimiters
 `(rainbow-delimiters-depth-1-face ((t (:foreground ,vmacs-labburn-fg))))
 `(rainbow-delimiters-depth-2-face ((t (:foreground ,vmacs-labburn-green+4))))
 `(rainbow-delimiters-depth-3-face ((t (:foreground ,vmacs-labburn-yellow-2))))
 `(rainbow-delimiters-depth-4-face ((t (:foreground ,vmacs-labburn-cyan))))
 `(rainbow-delimiters-depth-5-face ((t (:foreground ,vmacs-labburn-green+2))))
 `(rainbow-delimiters-depth-6-face ((t (:foreground ,vmacs-labburn-blue+1))))
 `(rainbow-delimiters-depth-7-face ((t (:foreground ,vmacs-labburn-yellow-1))))
 `(rainbow-delimiters-depth-8-face ((t (:foreground ,vmacs-labburn-green+1))))
 `(rainbow-delimiters-depth-9-face ((t (:foreground ,vmacs-labburn-blue-2))))
 `(rainbow-delimiters-depth-10-face ((t (:foreground ,vmacs-labburn-orange))))
 `(rainbow-delimiters-depth-11-face ((t (:foreground ,vmacs-labburn-green))))
 `(rainbow-delimiters-depth-12-face ((t (:foreground ,vmacs-labburn-blue-5))))
 `(rainbow-delimiters-unmatched-face ((t (:foreground "red"))))

 ;; --- re-builder
 `(reb-match-0 ((t (:foreground ,vmacs-bg :background ,vmacs-labburn-magenta))))
 `(reb-match-1 ((t (:foreground ,vmacs-bg :background ,vmacs-labburn-blue))))
 `(reb-match-2 ((t (:foreground ,vmacs-bg :background ,vmacs-labburn-orange))))
 `(reb-match-3 ((t (:foreground ,vmacs-bg :background ,vmacs-labburn-red))))

 ;; --- regex-tool
 `(regex-tool-matched-face ((t (:background ,vmacs-labburn-blue-4))))

 ;; --- realgud
 `(realgud-overlay-arrow1 ((t (:foreground ,vmacs-labburn-green))))
 `(realgud-overlay-arrow2 ((t (:foreground ,vmacs-labburn-yellow))))
 `(realgud-overlay-arrow3 ((t (:foreground ,vmacs-labburn-orange))))
 `(realgud-bp-enabled-face ((t (:inherit error))))
 `(realgud-bp-disabled-face ((t (:inherit secondary-selection))))
 `(realgud-bp-line-enabled-face ((t (:box (:color ,vmacs-labburn-red :style nil)))))
 `(realgud-bp-line-disabled-face ((t (:inherit secondary-selection))))
 `(realgud-bp-line-disabled-face ((t (:box (:color "grey70" :style nil)))))
 `(realgud-line-number ((t (:foreground ,vmacs-labburn-yellow))))
 `(realgud-backtrace-number ((t (:foreground ,vmacs-labburn-yellow))))

 ;; --- rtags
 `(rtags-errline ((t (:underline (:color "red" :style wave)))))
 `(rtags-fixitline ((t (:underline (:color "#93E0E3" :style wave)))))
 `(rtags-warnline ((t (:underline (:color "orange" :style wave)))))

 ;; --- ruby
 `(enh-ruby-op-face ((t (:inherit default))))
 `(enh-ruby-string-delimiter-face ((t (:inherit font-lock-string-face))))
 `(erm-syn-errline ((t (:underline (:color "red" :style wave)))))
 `(erm-syn-warnline ((t (:underline (:color "orange" :style wave)))))

 ;; --- sh-mode
 `(sh-heredoc     ((t (:foreground ,vmacs-labburn-yellow :weight bold))))
 `(sh-quoted-exec ((t (:foreground ,vmacs-labburn-red))))

 ;; --- smartparens
 `(sp-show-pair-enclosing-face ((t (:foreground ,vmacs-labburn-highlight))))
 `(sp-show-pair-match-face ((t (:foreground ,vmacs-labburn-highlight))))
 `(sp-show-pair-mismatch-face ((t (:foreground ,vmacs-labburn-red))))

 ;; --- sml-mode-line
 '(sml-modeline-end-face ((t :inherit default :width condensed)))

 ;; --- solaire
 `(solaire-default-face ((t (:inherit default :background ,vmacs-labburn-bg-05))))
 `(solaire-minibuffer-face ((t (:inherit default :background ,vmacs-labburn-bg-05))))
 `(solaire-hl-line-face ((t (:inherit hl-line :background ,vmacs-bg))))
 `(solaire-org-hide-face ((t (:inherit org-hide :background ,vmacs-labburn-bg-05))))

 ;; --- selectrum
 `(selectrum-current-candidate ((t (:foreground ,vmacs-labburn-yellow :weight bold :underline t))))
 `(selectrum-primary-highlight ((t (:background ,vmacs-labburn-green-2))))
 `(selectrum-secondary-highlight ((t (:background ,vmacs-labburn-green))))

 ;; --- SLIME
 `(slime-repl-output-face ((t (:foreground ,vmacs-labburn-red))))
 `(slime-repl-inputed-output-face ((t (:foreground ,vmacs-labburn-green))))
 `(slime-error-face
   ((((supports :underline (:style wave)))
     (:underline (:style wave :color ,vmacs-labburn-red)))
    (t
     (:underline ,vmacs-labburn-red))))
 `(slime-warning-face
   ((((supports :underline (:style wave)))
     (:underline (:style wave :color ,vmacs-labburn-orange)))
    (t
     (:underline ,vmacs-labburn-orange))))
 `(slime-style-warning-face
   ((((supports :underline (:style wave)))
     (:underline (:style wave :color ,vmacs-labburn-yellow)))
    (t
     (:underline ,vmacs-labburn-yellow))))
 `(slime-note-face
   ((((supports :underline (:style wave)))
     (:underline (:style wave :color ,vmacs-labburn-green)))
    (t
     (:underline ,vmacs-labburn-green))))
 `(slime-highlight-face ((t (:inherit highlight))))

 ;; --- sx
 `(sx-custom-button
   ((t (:background ,vmacs-labburn-fg :foreground ,vmacs-labburn-bg-1
                    :box (:line-width 3 :style released-button) :height 0.9))))
 `(sx-question-list-answers
   ((t (:foreground ,vmacs-labburn-green+3
                    :height 1.0 :inherit sx-question-list-parent))))
 `(sx-question-mode-accepted
   ((t (:foreground ,vmacs-labburn-green+3
                    :height 1.3 :inherit sx-question-mode-title))))
 '(sx-question-mode-content-face ((t (:inherit highlight))))
 `(sx-question-mode-kbd-tag
   ((t (:box (:color ,vmacs-labburn-bg-1 :line-width 3 :style released-button)
             :height 0.9))))

 ;; --- swiper
 `(swiper-isearch-current-match ((t (:foreground ,vmacs-bg :background ,vmacs-labburn-blue-1))))
 `(swiper-line-face ((t (nil))))

 ;; --- term
 `(term-color-black ((t (:foreground ,vmacs-bg :background ,vmacs-labburn-bg-1))))
 `(term-color-red ((t (:foreground ,vmacs-labburn-red-2 :background ,vmacs-labburn-red-4))))
 `(term-color-green ((t (:foreground ,vmacs-labburn-green :background ,vmacs-labburn-green+2))))
 `(term-color-yellow ((t (:foreground ,vmacs-labburn-orange :background ,vmacs-labburn-yellow))))
 `(term-color-blue ((t (:foreground ,vmacs-labburn-blue-1 :background ,vmacs-labburn-blue-4))))
 `(term-color-magenta ((t (:foreground ,vmacs-labburn-magenta :background ,vmacs-labburn-red))))
 `(term-color-cyan ((t (:foreground ,vmacs-labburn-cyan :background ,vmacs-labburn-blue))))
 `(term-color-white ((t (:foreground ,vmacs-labburn-fg :background ,vmacs-labburn-fg-1))))
 '(term-default-fg-color ((t (:inherit term-color-white))))
 '(term-default-bg-color ((t (:inherit term-color-black))))

 ;; --- ts
 `(ts-block-face ((t (:inherit font-lock-keyword-face))) t)
 `(ts-html-face ((t (:foreground ,vmacs-labburn-red))))

 ;; --- undo-tree
 `(undo-tree-visualizer-active-branch-face ((t (:foreground ,vmacs-labburn-fg+1))))
 `(undo-tree-visualizer-current-face ((t (:foreground ,vmacs-labburn-red-1))))
 `(undo-tree-visualizer-default-face ((t (:foreground ,vmacs-labburn-fg))))
 `(undo-tree-visualizer-register-face ((t (:foreground ,vmacs-labburn-yellow))))
 `(undo-tree-visualizer-unmodified-face ((t (:foreground ,vmacs-labburn-cyan))))

 ;; --- visual-regexp
 `(vr/group-0 ((t (:foreground ,vmacs-bg :background ,vmacs-labburn-green))))
 `(vr/group-1 ((t (:foreground ,vmacs-bg :background ,vmacs-labburn-orange))))
 `(vr/group-2 ((t (:foreground ,vmacs-bg :background ,vmacs-labburn-blue))))
 `(vr/match-0 ((t (:inherit isearch))))
 `(vr/match-1 ((t (:foreground ,vmacs-labburn-yellow-2 :background ,vmacs-labburn-bg-1))))
 `(vr/match-separator-face ((t (:foreground ,vmacs-labburn-red))))

 ;; --- volatile-highlights
 `(vhl/default-face ((t (:inherit highlight))))

 ;; --- web-mode
 `(web-mode-builtin-face ((t (:inherit ,font-lock-builtin-face))))
 `(web-mode-comment-face ((t (:inherit ,font-lock-comment-face))))
 `(web-mode-constant-face ((t (:inherit ,font-lock-constant-face))))
 `(web-mode-css-at-rule-face ((t (:foreground ,vmacs-labburn-orange ))))
 `(web-mode-css-prop-face ((t (:foreground ,vmacs-labburn-orange))))
 `(web-mode-css-pseudo-class-face ((t (:foreground ,vmacs-labburn-green+3))))
 `(web-mode-css-rule-face ((t (:foreground ,vmacs-labburn-blue))))
 `(web-mode-doctype-face ((t (:inherit ,font-lock-comment-face))))
 `(web-mode-folded-face ((t (:underline t))))
 `(web-mode-function-name-face ((t (:foreground ,vmacs-labburn-blue))))
 `(web-mode-html-attr-name-face ((t (:foreground ,vmacs-labburn-orange))))
 `(web-mode-html-attr-value-face ((t (:inherit ,font-lock-string-face))))
 `(web-mode-html-tag-face ((t (:foreground ,vmacs-labburn-cyan))))
 `(web-mode-keyword-face ((t (:inherit ,font-lock-keyword-face))))
 `(web-mode-preprocessor-face ((t (:inherit ,font-lock-preprocessor-face))))
 `(web-mode-string-face ((t (:inherit ,font-lock-string-face))))
 `(web-mode-type-face ((t (:inherit ,font-lock-type-face))))
 `(web-mode-variable-name-face ((t (:inherit ,font-lock-variable-name-face))))
 `(web-mode-server-background-face ((t (:background ,vmacs-bg))))
 `(web-mode-server-comment-face ((t (:inherit web-mode-comment-face))))
 `(web-mode-server-string-face ((t (:inherit web-moaivyde-string-face))))
 `(web-mode-symbol-face ((t (:inherit font-lock-constant-face))))
 `(web-mode-warning-face ((t (:inherit font-lock-warning-face))))
 `(web-mode-whitespaces-face ((t (:background ,vmacs-labburn-red))))
 `(web-mode-function-call-face ((t (:inherit default))))

 ;; --- whitespace-mode
 `(whitespace-space ((t (:background ,vmacs-labburn-bg+1 :foreground ,vmacs-labburn-bg+1))))
 `(whitespace-hspace ((t (:background ,vmacs-labburn-bg+1 :foreground ,vmacs-labburn-bg+1))))
 `(whitespace-tab ((t (:background ,vmacs-labburn-red-1))))
 `(whitespace-newline ((t (:foreground ,vmacs-labburn-bg+1))))
 `(whitespace-trailing ((t (:background ,vmacs-labburn-red))))
 `(whitespace-line ((t (:background ,vmacs-bg :foreground ,vmacs-labburn-magenta))))
 `(whitespace-space-before-tab ((t (:background ,vmacs-labburn-orange :foreground ,vmacs-labburn-orange))))
 `(whitespace-indentation ((t (:background ,vmacs-labburn-yellow :foreground ,vmacs-labburn-red))))
 `(whitespace-empty ((t (:background ,vmacs-labburn-yellow))))
 `(whitespace-space-after-tab ((t (:background ,vmacs-labburn-yellow :foreground ,vmacs-labburn-red))))

 ;; --- which-func-mode
 `(which-func ((t (:foreground ,vmacs-labburn-green+4))))

 ;; --- xcscope
 `(cscope-file-face ((t (:foreground ,vmacs-labburn-yellow))))
 `(cscope-function-face ((t (:foreground ,vmacs-labburn-cyan))))
 `(cscope-line-number-face ((t (:foreground ,vmacs-labburn-red))))
 `(cscope-mouse-face ((t (:foreground ,vmacs-bg :background ,vmacs-labburn-blue+1))))
 `(cscope-separator-face ((t (:foreground ,vmacs-labburn-red :underline t :overline t))))
 )

(set-face-attribute 'fringe nil :background nil)
(fringe-mode 10)

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'vmacs-labburn)
;;; vmacs-labburn-theme.el ends here
