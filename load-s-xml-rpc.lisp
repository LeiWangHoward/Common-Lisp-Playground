
(in-package :cs325-user)

#+allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :asdf))

#+allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew (merge-pathnames "s-xml/" *cs325-home*)
           asdf:*central-registry* :test #'equal)
  
  (pushnew (merge-pathnames "s-xml-rpc/" *cs325-home*)
           asdf:*central-registry* :test #'equal)
  
  (asdf:operate 'asdf:load-op 's-xml)
  (asdf:operate 'asdf:load-op 's-xml-rpc)
  
  (use-package :s-xml-rpc))

#-allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload "s-xml-rpc")
  (use-package :s-xml-rpc))
