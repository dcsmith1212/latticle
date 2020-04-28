(in-package :com.dsmith.fst)

(defclass cl-arc ()
  ((ilabel
    :initarg :ilabel
    :accessor ilabel
    :documentation "The input label for the arc.")
   (olabel
    :initarg :olabel
    :accessor olabel
    :documentation "The output label for the arc.")
   (weight
    :initarg :weight
    :accessor weight
    :documentation "The weight for the arc.")
   (nextstate
    :initarg :nextstate
    :accessor nextstate
    :documentation "The destination state for the arc.")))

(defmethod print-object ((object cl-arc) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (ilabel olabel weight nextstate) object
      (format stream "~s:~s/~d :nextstate ~d" ilabel olabel weight nextstate))))


(defclass cl-fst ()
  ((start-state
    :initform nil
    :documentation "The start state of the FST")
   (final-states
    :initarg :final-states
    :accessor final-states
    :initform ()
    :documentation "The list of final states of the FST")
   (states
    :accessor states
    :initform ()
    :documentation "The list of states in the FST")))

(defmethod print-object ((object cl-fst) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (states) object
      (format stream "~%")
      (dolist (state-id (state-ids object))
        (let ((final-weight (second (get-final object state-id)))
              (arcs (second (get-state state-id object))))
          (format stream ":state ~S " state-id)
          (if final-weight
              (format stream ":final-weight ~S~%" final-weight)
              (format stream "~%"))
          (dolist (arc arcs)
            (format stream "   ~S~%" arc)))))))


(defgeneric state-ids (fst))

(defgeneric max-state-id (fst))

(defgeneric get-state (state-id fst))

(defgeneric add-state (fst &optional id))

(defgeneric get-final (fst state-id))

(defgeneric set-final (fst state-id weigt))

(defgeneric add-arc (fst state-id arc))


(defmethod start-state ((fst cl-fst))
  (slot-value fst 'start-state))

(defmethod (setf start-state) (state-id (fst cl-fst))
  (add-state fst state-id)
  (setf (slot-value fst 'start-state) state-id))

(defmethod state-ids ((fst cl-fst))
  (mapcar #'first (states fst)))

(defmethod max-state-id ((fst cl-fst))
  (cond ((states fst) (apply 'max (state-ids fst)))
        (t '-1)))

(defmethod get-state (state-id (fst cl-fst))
  (car (member state-id (states fst) :key #'car)))

(defmethod add-state ((fst cl-fst) &optional id)
  (let ((new-state-id (or id (1+ (max-state-id fst)))))
    (unless (get-state new-state-id fst)
      (let ((new-state `(,new-state-id ())))
        (setf (states fst) (append (states fst) `(,new-state)))
        new-state))))

(defmethod get-final ((fst cl-fst) state-id)
  (car (member state-id (final-states fst) :key #'car)))

(defmethod set-final ((fst cl-fst) state-id weight)
  (add-state fst state-id)
  (let ((current-final (get-final fst state-id))
        (new-final `(,state-id ,weight)))
    (if current-final
        (setf (second current-final) (second new-final))
        (setf (final-states fst) (append (final-states fst) `(,new-final))))
    new-final))

(defmethod add-arc ((fst cl-fst) state-id arc)
  (add-state fst state-id)
  (let ((state (get-state state-id fst)))
    (setf (cdr state) `(,(cons arc (second state))))
    arc))





(defun add-arc-line (line fst)
  (let* ((arc-args (mapcan #'list '(:nextstate :ilabel :olabel :weight) (cdr line)))
         (arc (apply #'make-instance 'cl-arc arc-args)))
    (add-arc fst (car line) arc)))

(defun add-final-line (line fst)
  (set-final fst (car line) (second line)))

(defun process-text-line (line fst)
  (cond
    ((eql 5 (length line)) (add-arc-line line fst))
    ((eql 2 (length line)) (add-final-line line fst))
    (t (error "Hit line of unexpected length."))))

(defun read-text-fst (filename)
  (let ((ifh (open filename :if-does-not-exist nil))
        (fst (make-instance 'cl-fst)))
    (when ifh
      (loop for line = (read-line ifh nil) while line do
            (let ((line-list (with-input-from-string (in line)
                               (loop for x = (read in nil nil)
                                     while x collect x))))
              (process-text-line line-list fst)))
      (close ifh))
    fst))





(defmacro appendf (place obj)
  `(setf ,place (append ,place ,obj)))




; TODO: Automatically add start state when first state is added to FST


(defgeneric get-arcs (state-id fst))

(defgeneric get-nextstates (state-id fst))

(defgeneric invert-labels-off-state (state-id fst))

(defgeneric breadth-first-traversal (fst modifier-fn))

(defgeneric invert-labels-off-state (state-id fst))

(defgeneric invert (fst))

(defgeneric project-arcs-off-state (state-id type fst))

(defgeneric project (fst type))



(defmethod get-arcs (state-id (fst cl-fst))
  (second (get-state state-id fst)))

(defmethod get-nextstates (state-id (fst cl-fst))
  (remove-duplicates
   (mapcar #'(lambda (arc) (nextstate arc))
           (get-arcs state-id fst))))

(defmethod breadth-first-traversal ((fst cl-fst) modifier-fn)
  (let ((state-queue `(,(start-state fst)))
         (states-visited `(,(start-state fst))))
     (labels ((get-unvisited-nextstates (state-id)
                (remove-if #'(lambda (x) (member x states-visited))
                           (get-nextstates state-id fst)))
              (visit-state (state-id)
                (let ((states-to-add (get-unvisited-nextstates state-id)))
                  (appendf state-queue states-to-add)
                  (appendf states-visited states-to-add)
                  (funcall modifier-fn state-id fst)
                  (when state-queue (visit-state (pop state-queue))))))
       (visit-state (pop state-queue)))
     states-visited))



(defun invert-arc-labels (arc)
  (let ((old-ilabel (ilabel arc)))
    (setf (ilabel arc) (olabel arc))
    (setf (olabel arc) old-ilabel)
    arc))

(defmethod invert-labels-off-state (state-id (fst cl-fst))
  (mapcar #'invert-arc-labels (get-arcs state-id fst)))

(defmethod invert ((fst cl-fst))
  (breadth-first-traversal fst #'invert-labels-off-state)
  mfst)


