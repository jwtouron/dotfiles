(defun my-add-to-local-project (&optional dir)
  (labels ((find-asd-dir (dir)
             (cond
               ((uiop:directory-files dir #p"*.asd")
                dir)
               ((uiop:pathname-equal dir (uiop:pathname-root dir))
                nil)
               (t
                (find-asd-dir (uiop:pathname-parent-directory-pathname dir))))))
    (let ((dir (or dir
                   (find-asd-dir (uiop:getcwd))
                   (uiop:getcwd))))
      (pushnew dir ql:*local-project-directories*))))
