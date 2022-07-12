;;;; Third-party contrib modules

(defvar *contrib-clone-command*
  (concat "git clone"
          " --depth 1"
          " https://github.com/stumpwm/stumpwm-contrib"
          " ~/.stumpwm.d/modules"))
(when (and *initializing*
           (not (uiop:directory-exists-p #p"~/.stumpwm.d/modules")))
  (run-shell-command *contrib-clone-command*))

;;; Gaps

(load-module "swm-gaps")
(setf swm-gaps:*inner-gaps-size* 5
      swm-gaps:*outer-gaps-size* 5
      swm-gaps:*head-gaps-size* 0)
(when *initializing*
  (swm-gaps:toggle-gaps))
