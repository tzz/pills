;;; pills.el --- Text as small SVG lozenges (pills)  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Teodor Zlatanov

;; Author: Teodor Zlatanov <tzz@lifelogs.com>
;; Maintainer: Teodor Zlatanov <tzz@lifelogs.com>
;; Version: 0.2.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: faces
;; URL: https://github.com/tzz/pills

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; `pills' renders a string as a rounded pill: an SVG image placed on
;; a `display' property over the plain text, so the kill ring, isearch,
;; `buffer-substring-no-properties' and text terminals still see the
;; words.  Without SVG the text is returned with a face instead.
;;
;; Colours come from a hue derived from a key (by default the text
;; itself), so the same key always gets the same pill.  Pass :key to
;; colour by something other than the label, for example an address's
;; domain so every address at one company shares a colour.
;;
;; Rendering is cached per (text key bg fg scale surface style) and the
;; cache is cleared when the default font size changes.
;;
;; The drawing primitives (`pills-draw-lozenge', `pills-text', the
;; colour helpers, `pills-canvas') are exported so other packages can
;; draw their own pills in the same material: a stadium with a slight
;; top-to-bottom gradient, a soft shadow and a hairline highlight, text
;; flat on top (nngmail and efrit use them when this library is loaded).  The canvas is
;; a whole number of character cells wide and painted in the surface
;; colour first, because some mode lines mishandle image transparency
;; and stretch images over text of another width.

;;; Code:

(require 'cl-lib)
(require 'color)
(require 'svg)
(require 'dom)
(require 'lisp-mnt)

(defgroup pills nil
  "Text rendered as small SVG lozenges."
  :group 'faces)

(defcustom pills-scale 1.0
  "Height of a pill as a fraction of the default line height.
0.8 gives a pill a little shorter than a text line, which reads as a
tag; 1.0 matches the line."
  :type 'number)

(defcustom pills-saturation 0.45
  "Saturation of the generated pill colours, 0..1."
  :type 'number)

(defcustom pills-background-lightness 0.88
  "Lightness of the generated pill background, 0..1."
  :type 'number)

(defcustom pills-foreground-lightness 0.18
  "Lightness of the generated pill text, 0..1.
Low enough that the text stays legible at small pill sizes; the hue
of the key still tints it."
  :type 'number)

(defface pills-fallback
  '((t :inherit font-lock-constant-face))
  "Face used for pill text when SVG is not available.")

(defvar pills--cache (make-hash-table :test 'equal)
  "Rendered pills keyed by (TEXT KEY BG FG SCALE SURFACE STYLE).")

(defvar pills--cache-key nil
  "Font height/width the cache was built for.")

(defun pills-available-p ()
  "Non-nil when pills can be drawn here."
  (and (display-graphic-p) (image-type-available-p 'svg)))

(defcustom pills-style 'raised
  "How pills are drawn.
`raised': a slight top-to-bottom gradient, a soft shadow beneath and a
hairline highlight along the top, so the pill stands off its surround;
the text is flat on top at full contrast.  `flat': one colour."
  :type '(choice (const raised) (const flat) (const radial)))

;;;; Colours

(defun pills-rgb (color)
  "COLOR (#rrggbb, #rgb or a name) as (R G B) in 0..1.
Hex is parsed here: `color-name-to-rgb' consults the display and in
batch gives wrong values for hex colours."
  (cond
   ((and (stringp color) (string-match "\\`#\\([0-9a-fA-F]\\{6\\}\\)\\'" color))
    (mapcar (lambda (i) (/ (string-to-number (substring color (1+ (* 2 i)) (+ 3 (* 2 i))) 16) 255.0)) '(0 1 2)))
   ((and (stringp color) (string-match "\\`#\\([0-9a-fA-F]\\{3\\}\\)\\'" color))
    (mapcar (lambda (i) (/ (string-to-number (substring color (1+ i) (+ 2 i)) 16) 15.0)) '(0 1 2)))
   (t (or (ignore-errors (color-name-to-rgb color)) '(0.5 0.5 0.5)))))

(defun pills-hex (rgb)
  "RGB (0..1 each) as #rrggbb."
  (apply #'color-rgb-to-hex (append rgb '(2))))

(defun pills-shade (color amount)
  "COLOR lightened (AMOUNT > 0) or darkened (AMOUNT < 0) in HSL lightness."
  (pcase-let* ((`(,r ,g ,b) (pills-rgb color))
               (`(,h ,s ,l) (color-rgb-to-hsl r g b)))
    (pills-hex (color-hsl-to-rgb h s (max 0.0 (min 1.0 (+ l amount)))))))

(defun pills-dark-p (color)
  "Whether COLOR is on the dark half of the lightness scale."
  (< (nth 2 (apply #'color-rgb-to-hsl (pills-rgb color))) 0.5))

(defun pills-hue-color (key light)
  "A stable colour for KEY as #rrggbb; LIGHT non-nil gives the pale variant."
  (let* ((h (/ (mod (sxhash-equal key) 360) 360.0))
         (rgb (color-hsl-to-rgb h pills-saturation
                                (if light
                                    pills-background-lightness
                                  pills-foreground-lightness))))
    (pills-hex rgb)))

;;;; Drawing primitives (shared with efrit and nngmail)

(defun pills-lozenge-path (x y w h)
  "Path data of a stadium at X Y, W by H: explicit arcs at both ends.
Some renderers drop `rx' on small `rect's; arcs always draw."
  (let ((r (/ h 2.0)))
    (format "M%.2f,%.2f H%.2f A%.2f,%.2f 0 0 1 %.2f,%.2f H%.2f A%.2f,%.2f 0 0 1 %.2f,%.2f Z"
            (+ x r) y (- (+ x w) r)
            r r (- (+ x w) r) (+ y h)
            (+ x r)
            r r (+ x r) y)))

(defvar pills--id 0 "Counter for unique ids within SVG documents.")

(defun pills-draw-lozenge (svg x y w h fill &optional style)
  "Append to SVG a stadium at X Y, W by H in FILL, drawn per STYLE.
STYLE defaults to `pills-style'.  `raised': a `linearGradient' lighter at
the top and darker at the bottom (within a few percent of FILL's
lightness, so text on it keeps
its contrast), a blurred darker copy offset downward as a shadow, and
a near-white hairline along the upper arc.  `flat': FILL alone.
`radial': FILL at the centre fading to transparent at the edge, for a
glow behind a picture rather than a tag behind text.  Draw text
afterwards; it sits flat on top.  Ids are unique per call."
  (pcase (or style pills-style)
    ('flat (svg-node svg 'path :d (pills-lozenge-path x y w h) :fill fill))
    ('radial
     (let* ((grad (format "pills-r%d" (cl-incf pills--id)))
            (defs (or (car (dom-by-tag svg 'defs))
                      (let ((d (dom-node 'defs))) (svg--append svg d) d))))
       (dom-append-child
        defs (dom-node 'radialGradient `((id . ,grad) (cx . "50%") (cy . "50%") (r . "50%"))
                       (dom-node 'stop `((offset . "0%") (stop-color . ,fill) (stop-opacity . "1")))
                       (dom-node 'stop `((offset . "55%") (stop-color . ,fill) (stop-opacity . "0.8")))
                       (dom-node 'stop `((offset . "100%") (stop-color . ,fill) (stop-opacity . "0")))))
       (svg-node svg 'path :d (pills-lozenge-path x y w h) :fill (format "url(#%s)" grad))))
    (_
     (let* ((n (cl-incf pills--id))
            (grad (format "pills-g%d" n))
            (blur (format "pills-b%d" n))
            (defs (or (car (dom-by-tag svg 'defs))
                      (let ((d (dom-node 'defs))) (svg--append svg d) d)))
            (dark-pill (pills-dark-p fill))
            (light (pills-shade fill (if dark-pill 0.14 0.07)))
            (dark (pills-shade fill (if dark-pill -0.10 -0.16)))
            (shadow (pills-shade fill -0.55))
            (highlight-opacity (if dark-pill "0.25" "0.6")))
       (dom-append-child
        defs (dom-node 'linearGradient `((id . ,grad) (x1 . "0") (y1 . "0") (x2 . "0") (y2 . "1"))
                       (dom-node 'stop `((offset . "0%") (stop-color . ,light)))
                       (dom-node 'stop `((offset . "100%") (stop-color . ,dark)))))
       (dom-append-child
        defs (dom-node 'filter `((id . ,blur) (x . "-10%") (y . "-30%") (width . "120%") (height . "180%"))
                       (dom-node 'feGaussianBlur `((stdDeviation . ,(format "%.2f" (* 0.09 h)))))))
       (svg-node svg 'path :d (pills-lozenge-path x (+ y (* 0.12 h)) w h)
                 :fill shadow :fill-opacity "0.6" :filter (format "url(#%s)" blur))
       (svg-node svg 'path :d (pills-lozenge-path x y w h) :fill (format "url(#%s)" grad))
       (let ((r (/ h 2.0)) (inset (* 0.08 h)))
         (svg-node svg 'path
                   :d (format "M%.2f,%.2f H%.2f" (+ x r) (+ y inset) (- (+ x w) r))
                   :fill "none" :stroke "#ffffff" :stroke-opacity highlight-opacity
                   :stroke-width (format "%.2f" (max 0.8 (* 0.06 h))) :stroke-linecap "round"))))))

(defun pills-text (svg text x y w h fg &optional family)
  "Draw TEXT flat and bold, centred in the box X Y W H on SVG, in FG.
FAMILY defaults to the default face's.  The font size is 0.6 of H and
the baseline is nudged for the font's descent, so it centres optically."
  (let* ((font-size (round (* 0.6 h)))
         (family (format "'%s', ui-monospace, Menlo, monospace"
                         (or family (face-attribute 'default :family) "monospace"))))
    (svg-text svg text :x (+ x (/ w 2.0)) :y (+ y (* h 0.5) (* font-size 0.35))
              :text-anchor "middle"
              :font-family family :font-weight "bold" :font-size font-size :fill fg)))

(defun pills-surface-background ()
  "The colour behind a pill in ordinary text: the default face's background."
  (let ((bg (face-attribute 'default :background nil t)))
    (if (and (stringp bg) (not (string= bg "unspecified-bg")))
        (pills-hex (pills-rgb bg))
      "#000000")))

;;;; The pill

(defun pills--maybe-reset-cache ()
  "Empty the cache when the default font metrics changed."
  (let ((key (cons (default-font-height) (default-font-width))))
    (unless (equal key pills--cache-key)
      (clrhash pills--cache)
      (setq pills--cache-key key))))

(cl-defun pills-canvas (columns bg draw &key (scale pills-scale) surface style)
  "An empty pill COLUMNS character cells wide in BG, with DRAW's content on it.
DRAW is called with (SVG X Y W H), the box of the lozenge, to add
whatever belongs inside: `pills' puts text there, other callers draw
pictures.  SCALE and SURFACE are as in `pills'; STYLE is passed to
`pills-draw-lozenge' (default `pills-style').  Returns the image
descriptor, sized to cover COLUMNS cells of the line."
  (let* ((lh (default-font-height))
         (style (or style pills-style))
         (pill-h (max 10 (round (* scale lh (if (eq style 'raised) 0.93 1.0)))))
         (cw (default-font-width))
         (width (* columns cw))
         (inset (/ cw 3.0))
         (top (/ (- lh pill-h) 2.0))
         (svg (svg-create width lh)))
    (dom-set-attribute svg 'viewBox (format "0 0 %s %s" width lh))
    (svg-rectangle svg 0 0 width lh :fill (or surface (pills-surface-background)))
    (pills-draw-lozenge svg inset top (- width (* 2 inset)) pill-h bg style)
    (funcall draw svg inset top (- width (* 2 inset)) pill-h)
    (svg-image svg :ascent 'center :scale 1)))

(defun pills--columns (text scale)
  "Character cells a pill of TEXT at SCALE covers.
The text plus one cell of air, or more when the font is wide enough
that the glyphs at pill size need it."
  (let* ((lh (default-font-height))
         (cw (default-font-width))
         (pill-h (max 10 (round (* scale lh (if (eq pills-style 'raised) 0.93 1.0)))))
         (glyph (* 0.62 (round (* 0.6 pill-h)))))
    (max (1+ (string-width text))
         (ceiling (/ (+ (* (string-width text) glyph) (* 2 cw)) (float cw))))))

(cl-defun pills-fit (text width &rest keys &key (scale pills-scale) &allow-other-keys)
  "TEXT as one pill exactly WIDTH columns wide, cut with an ellipsis to fit.
KEYS (including SCALE) are passed to `pills'.  For table-like buffers
\(a Gnus summary) where every line must put the next column at the same
place: the pill spans the whole cell whatever the text length.  Without
SVG, TEXT itself is cut and padded to WIDTH."
  (if (not (pills-available-p))
      (truncate-string-to-width text width nil ?\s "…")
    (let ((label text))
      (while (and (> (pills--columns label scale) width)
                  (> (string-width label) 1))
        (setq label (truncate-string-to-width label (1- (string-width label)) nil nil "…")))
      (apply #'pills label :columns width keys))))

(cl-defun pills (text &key key bg fg (scale pills-scale) help-echo surface columns)
  "Return TEXT rendered as a pill.
KEY selects the colour; default is TEXT.  BG and FG are #rrggbb strings
that override the derived colours.  SCALE is the pill height relative to
the line height.  HELP-ECHO, if given, is attached as a tooltip.
COLUMNS fixes the pill's width in character cells (default: as many as
the text needs); the text is centred in it.
SURFACE is the colour the canvas is painted in behind the pill (default
the default face's background): a renderer that mishandles transparency
then still shows the right corners.  The returned string is TEXT with a
`display' image property, or TEXT with `pills-fallback' when SVG is
unavailable."
  (pills--maybe-reset-cache)
  (let* ((key (or key text))
         (surface (or surface (pills-surface-background)))
         (cache-key (list text key bg fg scale surface columns pills-style))
         (rendered
          (or (gethash cache-key pills--cache)
              (puthash
               cache-key
               (if (not (pills-available-p))
                   (propertize text 'face 'pills-fallback)
                 (let* ((columns (max (or columns 0) (pills--columns text scale)))
                        (fg (or fg (pills-hue-color key nil))))
                   (propertize (truncate-string-to-width text columns nil ?\s)
                               'display
                               (pills-canvas columns (or bg (pills-hue-color key t))
                                                (lambda (svg x y w h)
                                                  (pills-text svg text x y w h fg))
                                                :scale scale :surface surface))))
               pills--cache))))
    (if help-echo
        (propertize rendered 'help-echo help-echo)
      rendered)))

;;;; Reloading

(defvar pills-reload-hook nil
  "Run after `pills-reload', once the new code is in and the caches are empty.
For packages that keep their own rendered pills (nngmail, efrit, a Gnus
line format): redraw there.")

;;;###autoload
(defun pills-reload ()
  "Reload pills.el from source and drop every rendered pill.
Loads the .el (not a stale .elc or .eln), empties `pills--cache' and
Emacs's image cache, so the next draw uses the new code and colours,
then runs `pills-reload-hook'.  Buffers that already hold pills keep
the old images until they are redrawn: in Gnus press \\=`L' in *Group*
or re-enter the summary; the mode line and tab line redraw by
themselves."
  (interactive)
  (let* ((source (locate-library "pills.el" t))
         (start (float-time)))
    (unless source
      (user-error "Pills-reload: cannot find pills.el on `load-path'"))
    ;; the full .el name: a bare "pills" would pick a newer .elc or .eln
    (load source nil t)
    (clrhash pills--cache)
    (setq pills--cache-key nil)
    ;; `clear-image-cache' exists only in an Emacs built with image support
    (when (fboundp 'clear-image-cache) (ignore-errors (funcall 'clear-image-cache t)))
    (force-mode-line-update t)
    (run-hooks 'pills-reload-hook)
    (message "pills %s reloaded from %s in %.0f ms"
             (or (lm-version source) "?")
             (abbreviate-file-name source)
             (* 1000 (- (float-time) start)))))

(provide 'pills)
;;; pills.el ends here
