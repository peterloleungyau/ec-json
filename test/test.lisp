(in-package :cl-user)

(defparameter x
    (loop for i from 0 below 10 by 0.1
          collect i))

(defparameter y
    (mapcar #'sin x))

(cl-plotly::write-to-html
 (list `(:x ,x
         :y ,y
         :mode "lines"))
 `(:title "Sin")
 "test1.html")
