(setq some-rule '(("000" . "0")
		  ("001" . "0")
		  ("010" . "0")
		  ("011" . "0")
		  ("100" . "0")
		  ("101" . "1")
		  ("110" . "0")
		  ("111" . "0")))


(defun apply-rule (neighborhood rule)
  "Return cell state by applying rule to three-cell neighborhood"
  (cdr (assoc neighborhood rule)))


(defun next-gen (gen rule)
  "Apply rule to row to produce next generation row!"
  (let ((new "")
	(old (concat (string (elt gen (- (length gen) 1)))
		     gen
		     (string (elt gen 0)))))
    (dotimes (i
	      (- (length old) 2))
      (setq new (concat new (apply-rule (format "%s%s%s"
						(string (elt old i))
						(string (elt old (+ i 1)))
						(string (elt old (+ i 2))))
					rule))))
    new))



(next-gen "1010" some-rule)
