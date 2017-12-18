(setq rule-30 '(("000" . "0")   ; 1
		("001" . "1")   ; 2
		("010" . "1")   ; 4
		("011" . "1")   ; 8
		("100" . "1")   ; 16
		("101" . "0")   ; 32
		("110" . "0")   ; 64
		("111" . "0"))) ; 128


(defun apply-rule (neighborhood rule)
  "Return cell state by applying rule to three-cell neighborhood"
  (cdr (assoc neighborhood rule)))


(defun next-gen (gen rule)
  "Apply rule to row to produce next generation row!"
  (let ((new "")
	(old (concat (string (elt gen (- (length gen) 1)))
		     gen
		     (string (elt gen 0)))))
    (dotimes (i (- (length old) 2))
      (setq new (concat new (apply-rule (format "%s%s%s"
						(string (elt old i))
						(string (elt old (+ i 1)))
						(string (elt old (+ i 2))))
					rule))))
    new))


;; an initial row
(setq row "0000000000000000000000000000000000001000000000000000000000000000000000000")

;; do 30 rows
(dotimes (i 60)
  (princ (format "%s\n" row))
  (setq row (next-gen row rule-30)))
