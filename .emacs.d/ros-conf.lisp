(defun my-add-to-local-project (&optional dir)
  (pushnew (or dir (uiop:getcwd)) ql:*local-project-directories*))
