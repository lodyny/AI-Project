(defun construct-node (board parent pieces &optional (g 0) (h 0))
  (list board parent g h pieces)
)
