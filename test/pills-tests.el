;;; pills-tests.el --- Tests for pills.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Teodor Zlatanov

;; This file is part of pills.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Run with: emacs -Q --batch -L . -L test -l test/pills-tests.el \
;;           -f ert-run-tests-batch-and-exit
;;
;; Batch Emacs has no display, so `pills-available-p' is stubbed and
;; `svg-image' is replaced by a function that keeps the SVG document, so
;; the tests can look at what was drawn.

;;; Code:

(require 'ert)
(require 'pills)

(defmacro pills-tests--with-display (&rest body)
  "Run BODY as if on a 12x24 px graphical display, keeping SVG documents."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'pills-available-p) (lambda () t))
             ((symbol-function 'pills--metrics) (lambda () (cons 24 12)))
             ((symbol-function 'pills-surface-background) (lambda () "#000000"))
             ((symbol-function 'svg-image)
              (lambda (svg &rest _) (list 'image :svg svg))))
     (clrhash pills--cache)
     ,@body))

(defun pills-tests--svg (string)
  "The SVG document behind the pill STRING."
  (plist-get (cdr (get-text-property 0 'display string)) :svg))

(ert-deftest pills-test-colors ()
  "Hex parsing, shading and stable hue keys."
  (should (equal '(1.0 0.0 0.0) (pills-rgb "#ff0000")))
  (should (equal '(1.0 1.0 1.0) (pills-rgb "#fff")))
  (should (equal "#ff0000" (pills-hex '(1.0 0.0 0.0))))
  (should (pills-dark-p "#102030"))
  (should-not (pills-dark-p "#f0f0f0"))
  (should (string< (pills-shade "#808080" -0.2) (pills-shade "#808080" 0.2)))
  (should (equal (pills-hue-color "example.com" t) (pills-hue-color "example.com" t)))
  ;; Distinct keys get distinct hues.  (One-character keys such as "a"
  ;; and "b" hash one apart and round to the same colour; real keys are
  ;; words and domains.)
  (should-not (equal (pills-hue-color "example.com" t)
                     (pills-hue-color "example.org" t))))

(ert-deftest pills-test-text-fallback ()
  "Without SVG the text comes back with the fallback face."
  (cl-letf (((symbol-function 'pills-available-p) (lambda () nil)))
    (clrhash pills--cache)
    (let ((p (pills "hello")))
      (should (equal "hello" p))
      (should (eq 'pills-fallback (get-text-property 0 'face p))))))

(ert-deftest pills-test-pill-covers-whole-cells ()
  "A pill covers a whole number of cells and its image is that wide."
  (pills-tests--with-display
    (let* ((p (pills "hello"))
           (svg (pills-tests--svg p)))
      (should (= (string-width p) (pills--columns "hello" pills-scale)))
      (should (equal (* 12 (string-width p)) (dom-attr svg 'width)))
      (should (equal 24 (dom-attr svg 'height)))
      ;; surface painted first, then the lozenge and the text
      (should (eq 'rect (dom-tag (car (dom-children svg)))))
      (should (equal "hello" (dom-text (car (dom-by-tag svg 'text))))))))

(ert-deftest pills-test-fixed-columns ()
  "`:columns' and `pills-fit' give a fixed width whatever the text."
  (pills-tests--with-display
    (dolist (text '("05:10" "Mon 22:52" "2026-10-13"))
      (let ((p (pills-fit text 11)))
        (should (= 11 (string-width p)))
        (should (equal 132 (dom-attr (pills-tests--svg p) 'width)))))
    ;; too long: cut with an ellipsis, still 11 columns
    (let ((p (pills-fit "a very long label indeed" 11)))
      (should (= 11 (string-width p)))
      (should (string-suffix-p "…" (string-trim (substring-no-properties p)))))))

(ert-deftest pills-test-styles ()
  "Each style draws what it promises."
  (pills-tests--with-display
    (let ((pills-style 'flat))
      (clrhash pills--cache)
      (should-not (dom-by-tag (pills-tests--svg (pills "x")) 'linearGradient)))
    (let ((pills-style 'raised))
      (clrhash pills--cache)
      (let ((svg (pills-tests--svg (pills "x"))))
        (should (dom-by-tag svg 'linearGradient))
        (should (dom-by-tag svg 'feGaussianBlur))))
    (let ((img (pills-canvas 3 "#336699" (lambda (svg x y w h)
                                          (svg-circle svg (+ x (/ w 2.0)) (+ y (/ h 2.0)) 2))
                             :style 'radial)))
      (should (dom-by-tag (plist-get (cdr img) :svg) 'radialGradient))
      (should (dom-by-tag (plist-get (cdr img) :svg) 'circle)))))

(ert-deftest pills-test-cache ()
  "Same arguments reuse the rendering; a font change empties the cache."
  (pills-tests--with-display
    (pills "a") (pills "a")
    (should (= 1 (hash-table-count pills--cache)))
    (pills "a" :columns 9)
    (should (= 2 (hash-table-count pills--cache)))
    (cl-letf (((symbol-function 'pills--metrics) (lambda () (cons 30 12))))
      (pills--maybe-reset-cache)
      (should (= 0 (hash-table-count pills--cache))))))

(provide 'pills-tests)
;;; pills-tests.el ends here
