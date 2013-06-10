(defmacro preserve (parms &body body)
  `((lambda ,parms ,@body) ,@parms))