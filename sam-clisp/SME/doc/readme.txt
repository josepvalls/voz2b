first, load config.lisp

(in-package :sme)
(load-sme t)
(sme-init)
(in-package :cl-user)
load simple-heat-flow.dgroup, and simple-water-flow.dgroup

(sme:match 'swater-flow 'sheat-flow T)



-----------------------

(gm-elements (first *gmaps*))

(expression-name (mh-base-item a))