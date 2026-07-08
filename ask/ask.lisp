;;;; ask.lisp — interactive "ask the user" tool extension for kli.
;;;
;;; Mirrors the @oh-my-pi/pi-coding-agent ask tool: the model calls this tool
;;; to ask the user design questions with multiple choices, a recommended
;;; option, and an "Other (type your own)" custom-input fallback. Choices are
;;; navigated with up/down; multiple questions are navigated with left/right.
;;;
;;; The questions payload is a JSON string (kli's tool parameter schema DSL is
;;; flat-only — no nested objects or arrays — so structured input rides in as a
;;; string, the same way cairn's task_query takes an s-expression as :query).
;;; JSON shape:
;;;   {"questions":[{"id":"...","question":"...",
;;;                  "options":[{"label":"...","description":"..."}, ...],
;;;                  "multi": false, "recommended": 0}, ...]}

(in-package #:kli/author)

;;; --- Parameter parsing -------------------------------------------------------

(defun ask-parse-questions (questions-json)
  "Parse the JSON \"questions\" string into a list of hash-tables, one per
question (jzon returns objects as EQUAL hash-tables, arrays as vectors).
Returns NIL when the string is empty or not an array."
  (when (or (null questions-json)
            (not (stringp questions-json))
            (zerop (length questions-json)))
    (return-from ask-parse-questions nil))
  (let ((parsed (com.inuoe.jzon:parse questions-json)))
    (unless (vectorp parsed)
      (return-from ask-parse-questions nil))
    (coerce parsed 'list)))

;;; --- Constants ---------------------------------------------------------------

(defparameter +ask-recommended-suffix+ " (Recommended)"
  "Suffix appended to the recommended option's label.")

(defparameter +ask-other-option+ "Other (type your own)"
  "Label of the custom-input option, always appended as the last row.")

(defparameter +ask-done-selecting+ "Done selecting"
  "Label of the row that closes a multi-select menu and returns the checked
options.")

(defparameter +ask-interceptor-id+ :ask-nav-interceptor
  "Route interceptor id used to capture left/right question navigation.")

;;; --- TUI app discovery -------------------------------------------------------

(defun ask-find-tui-app (context)
  "Find the live TUI app object in CONTEXT's registry by type (not by id, which
is generated per session). Returns the app or NIL."
  (let ((registry (kli:context-registry context)))
    (when registry
      (let ((found nil))
        (maphash (lambda (id obj)
                   (declare (ignore id))
                   (when (typep obj 'kli/tui/app:tui-app)
                     (push obj found)))
                 (slot-value registry 'kli::objects))
        (first found)))))

;;; --- Option / question accessors --------------------------------------------

(defun ask-option-label (option)
  "Read the \"label\" string from a parsed option hash-table."
  (gethash "label" option))

(defun ask-option-description (option)
  "Read the optional \"description\" string from a parsed option, or NIL."
  (let ((d (gethash "description" option)))
    (and (stringp d) (plusp (length d)) d)))

(defun ask-question-options (question)
  "Return the question's options vector as a list, or NIL. Accepts either
\"options\" or \"choices\" as the key (the tool description says \"options\"
but callers may use either)."
  (let ((opts (or (gethash "options" question)
                  (gethash "choices" question))))
    (and (vectorp opts) (coerce opts 'list))))

(defun ask-question-recommended (question)
  "Return the recommended option index for QUESTION, or NIL."
  (let ((r (gethash "recommended" question)))
    (and (integerp r) (>= r 0) r)))

(defun ask-question-multi-p (question)
  "True when QUESTION has \"multi\" set to true."
  (let ((m (gethash "multi" question)))
    (eq m t)))

(defun ask-question-id (question)
  "Read the \"id\" string from a parsed question, or a fallback."
  (or (gethash "id" question) "question"))

(defun ask-strip-recommended-suffix (label)
  "Remove the \" (Recommended)\" suffix from LABEL if present."
  (let ((suffix +ask-recommended-suffix+))
    (if (and (stringp label)
             (>= (length label) (length suffix))
             (string= label suffix
                      :start1 (- (length label) (length suffix))))
        (subseq label 0 (- (length label) (length suffix)))
        label)))

(defun ask-string-ends-with (string suffix)
  "True when STRING ends with SUFFIX."
  (and (stringp string) (stringp suffix)
       (>= (length string) (length suffix))
       (string= string suffix
                :start1 (- (length string) (length suffix)))))

;;; --- Menu rows (single-select) -----------------------------------------------

(defun ask-menu-rows (question)
  "Build open-tui-app-menu rows from a parsed question: one row per option
with \"(Recommended)\" appended to the recommended option's label, plus the
\"Other (type your own)\" row as the last entry. Each row carries the display
label as :insert, the description, and the clean label (suffix stripped) as
:value so the caller receives the plain label. The Other row's :value is :other."
  (let ((recommended (ask-question-recommended question))
        (options (ask-question-options question)))
    (append
     (loop for option in options
           for label = (ask-option-label option)
           for desc = (ask-option-description option)
           for index from 0
           for display = (if (and recommended (= index recommended)
                                  (not (ask-string-ends-with
                                        label +ask-recommended-suffix+)))
                             (concatenate 'string label
                                          +ask-recommended-suffix+)
                             label)
           collect (list :insert display
                         :description desc
                         :value label))
     (list (list :insert +ask-other-option+
                 :description nil
                 :value :other)))))

;;; --- Menu rows (multi-select) -----------------------------------------------

(defun ask-multi-menu-rows (question checked)
  "Build menu rows for a multi-select question: each option label prefixed
with a checkbox marker ([x] for checked, [ ] for unchecked), followed by
\"Done selecting\" and \"Other (type your own)\". CHECKED is a list of
currently checked labels."
  (let ((options (ask-question-options question))
        (checked-set (make-hash-table :test 'equal)))
    (dolist (label checked)
      (setf (gethash label checked-set) t))
    (append
     (loop for option in options
           for label = (ask-option-label option)
           for desc = (ask-option-description option)
           for mark = (if (gethash label checked-set) "[x] " "[ ] ")
           collect (list :insert (concatenate 'string mark label)
                         :description desc
                         :value label))
     (list (list :insert +ask-done-selecting+
                 :description "finish and return the checked options"
                 :value :done))
     (list (list :insert +ask-other-option+
                 :description nil
                 :value :other)))))

;;; --- Custom text input (Other) ----------------------------------------------

(defun ask-prompt-for-other (app question on-custom on-cancel)
  "Swap the editor prompt and on-submit so the user can type a custom answer.
ON-CUSTOM is called with the typed text on Enter; ON-CANCEL is called if the
user submits empty text. Restores the original prompt and on-submit after."
  (let* ((editor (kli/tui/app:tui-app-editor app))
         (old-prompt (kli/tui/editor:editor-prompt editor))
         (old-on-submit (kli/tui/editor:editor-on-submit editor))
         (prompt-text (gethash "question" question)))
    (setf (kli/tui/editor:editor-prompt editor)
          (format nil "~A: " prompt-text)
          (kli/tui/editor:editor-value editor) ""
          (kli/tui/editor:editor-on-submit editor)
          (lambda (text)
            (setf (kli/tui/editor:editor-prompt editor) old-prompt
                  (kli/tui/editor:editor-on-submit editor) old-on-submit)
            (if (and (stringp text) (plusp (length text)))
                (funcall on-custom text)
                (funcall on-cancel))))
    (kli/tui/app:render-tui-app app)))

;;; --- Notice / question indicator -------------------------------------------

(defun ask-set-notice (app text)
  "Show TEXT as the hint line just above the prompt (the renderer notice).
Pass NIL to clear it."
  (setf (kli/tui/transcript:scrollback-renderer-notice
         (kli/tui/app:tui-app-renderer app))
        text)
  (setf (kli/tui/app:tui-app-notice-expires-at app) nil))

(defun ask-make-notice (question index count)
  "Build the notice text: the question text prefixed with the position
indicator (e.g. 'Question 1/3: ...'). For a single question, just the text."
  (let ((qtext (gethash "question" question)))
    (if (> count 1)
        (format nil "Question ~D/~D: ~A" (1+ index) count qtext)
        qtext)))

;;; --- Single-select menu ------------------------------------------------------

(defun ask-open-single-menu (app question on-result notice)
  "Open a selection menu for QUESTION on the TUI loop thread. ON-RESULT is a
callback invoked with the chosen label string on Enter, with (list :custom
text) when Other is used, and with NIL on Esc. NOTICE is shown above the
prompt. Returns immediately after opening; the caller blocks on a semaphore
that ON-RESULT signals."
  (let ((rows (ask-menu-rows question)))
    (kli/tui/app:call-on-main-thread-task
     app
     (lambda ()
       (ask-set-notice app notice)
       (kli/tui/app:open-tui-app-menu
        app
        (loop for row in rows
              collect (list :insert (getf row :insert)
                            :description (or (getf row :description)
                                             (if (eq (getf row :value) :other)
                                                 "type your own answer"
                                                 ""))
                            :value (getf row :value)))
        (lambda (choice)
          (cond
            ((eq choice :other)
             (ask-prompt-for-other app question
                                   (lambda (text)
                                     (funcall on-result (list :custom text)))
                                   (lambda ()
                                     (funcall on-result nil))))
            (t
             (funcall on-result choice)))))
       (kli/tui/app:render-tui-app app)))))

;;; --- Multi-select menu -------------------------------------------------------

(defun ask-open-multi-menu (app question on-result notice)
  "Open a multi-select menu for QUESTION on the TUI loop thread. Toggling an
option re-opens the menu with updated checkboxes; \"Done selecting\" calls
ON-RESULT with (list :multi checked-labels); \"Other\" prompts for custom
text and calls ON-RESULT with (list :custom text); Esc calls ON-RESULT with
NIL. NOTICE is shown above the prompt. Returns immediately after opening;
the caller blocks on a semaphore."
  (labels
      ((open-with (checked)
                  (kli/tui/app:call-on-main-thread-task
                   app
                   (lambda ()
                     (ask-set-notice app notice)
                     (kli/tui/app:open-tui-app-menu
                      app
                      (loop for row in (ask-multi-menu-rows question checked)
                            collect (list :insert (getf row :insert)
                                          :description (or (getf row :description) "")
                                          :value (getf row :value)))
                      (lambda (choice)
                        (cond
                          ((eq choice :done)
                           (funcall on-result (list :multi checked)))
                          ((eq choice :other)
                           (ask-prompt-for-other app question
                                                 (lambda (text)
                                                   (funcall on-result (list :custom text)))
                                                 (lambda ()
                                                   (funcall on-result nil))))
                          (t
                           (let ((new-checked
                                  (if (member choice checked :test 'equal)
                                      (remove choice checked :test 'equal)
                                      (append checked (list choice)))))
                             (open-with new-checked))))))
                     (kli/tui/app:render-tui-app app)))))
    (open-with nil)))

;;; --- Result formatting -------------------------------------------------------

(defun ask-format-result (result)
  "Format the selection result for the tool output text."
  (cond
    ((null result) "User cancelled the selection.")
    ((and (consp result) (eq (car result) :custom))
     (format nil "User provided custom input: ~A" (second result)))
    ((and (consp result) (eq (car result) :multi))
     (format nil "User selected: ~{~A~^, ~}" (second result)))
    (t (format nil "User selected: ~A" result))))

(defun ask-format-question-result (question result)
  "Format one question's answer line for the tool output."
  (let ((id (ask-question-id question)))
    (cond
      ((null result) (format nil "~A: (cancelled)" id))
      ((and (consp result) (eq (car result) :custom))
       (format nil "~A: \"~A\"" id (second result)))
      ((and (consp result) (eq (car result) :multi))
       (format nil "~A: [~{~A~^, ~}]" id (second result)))
      (t (format nil "~A: ~A" id result)))))

(defun ask-format-all-results (questions answers)
  "Format the full multi-question result text."
  (if (= (length questions) 1)
      (ask-format-result (first answers))
      (format nil "User answers:~%~{  ~A~^~%~}"
              (loop for q in questions
                    for a in answers
                    collect (ask-format-question-result q a)))))

;;; --- Multi-question navigation ----------------------------------------------

(defun ask-make-nav-interceptor (protocol on-nav)
  "Return a route interceptor function that captures left/right key events
and calls ON-NAV with :back or :forward. Other events fall through. The
interceptor signature is (app event) per add-tui-app-route-interceptor."
  (lambda (app event)
    (declare (ignore app))
    (let ((key-id (kli/tui/input:input-event-key-id event)))
      (let ((action (and key-id (kli/tui/keymap:keymap-action protocol key-id))))
        (cond
          ((eq action :move-char-left)
           (funcall on-nav :back)
           :handled)
          ((eq action :move-char-right)
           (funcall on-nav :forward)
           :handled)
          (t nil))))))

(defun ask-open-menu-for (app question on-result index count)
  "Dispatch to the single- or multi-select menu based on the question's
\"multi\" flag. INDEX and COUNT build the position notice."
  (let ((notice (ask-make-notice question index count)))
    (if (ask-question-multi-p question)
        (ask-open-multi-menu app question on-result notice)
        (ask-open-single-menu app question on-result notice))))

(defun ask-run-questions (app questions)
  "Run the ask flow for a list of parsed questions. Opens a menu per question;
left/right (captured via a route interceptor) navigates between questions,
preserving prior answers. Returns all answers after the last is answered or
right is pressed on it."
  (let* ((protocol (kli:object-protocol app))
         (count (length questions))
         (answers (make-array count :initial-element nil))
         (index 0)
         (sem (sb-thread:make-semaphore))
         (signal nil)
         (interceptor
          (ask-make-nav-interceptor protocol
                                    (lambda (dir)
                                      (setf signal dir)
                                      (sb-thread:signal-semaphore sem)))))
    (labels
        ((answer-callback ()
           (lambda (choice)
             (setf signal (list :answer choice))
             (sb-thread:signal-semaphore sem)))
         (open-current ()
                       (ask-open-menu-for app (nth index questions) (answer-callback)
                                          index count)))
      (kli/tui/app:call-on-main-thread-task
       app
       (lambda ()
         (kli/tui/app:add-tui-app-route-interceptor app +ask-interceptor-id+ interceptor)))
      (unwind-protect
           (progn
             (open-current)
             (loop
               (sb-thread:wait-on-semaphore sem)
               (cond
                 ((eq signal :back)
                  (when (> index 0)
                    (decf index))
                  (open-current))
                 ((eq signal :forward)
                  (if (< index (- count 1))
                      (progn
                        (incf index)
                        (open-current))
                      (return)))
                 ((and (consp signal) (eq (first signal) :answer))
                  (setf (aref answers index) (second signal))
                  (if (< index (- count 1))
                      (progn
                        (incf index)
                        (open-current))
                      (return)))
                 (t (return)))))
        (kli/tui/app:call-on-main-thread-task
         app
         (lambda ()
           (kli/tui/app:remove-tui-app-route-interceptor app +ask-interceptor-id+)
           (ask-set-notice app nil)))))
    (kli/ext:make-tool-result
     :content (list (kli/ext:make-tool-text-content
                    (ask-format-all-results questions
                                           (coerce answers 'list)))))))

;;; --- Runner ------------------------------------------------------------------

(defun ask-run (tool parameters context &key call-id on-update)
  (declare (ignore tool call-id on-update))
  (let ((questions-json (kli/ext:tool-parameter parameters :questions)))
    (let ((questions (ask-parse-questions questions-json)))
      (cond
        ((null questions)
         (kli/ext:make-tool-result
          :content (list (kli/ext:make-tool-text-content
                         "Error: \"questions\" must be a non-empty JSON array."))))
        ((null context)
         (kli/ext:make-tool-result
          :content (list (kli/ext:make-tool-text-content
                         "Ask needs a session context."))))
        (t
         (let ((app (ask-find-tui-app context)))
           (if (null app)
               (kli/ext:make-tool-result
                :content (list (kli/ext:make-tool-text-content
                               "Ask needs the interactive terminal.")))
               (ask-run-questions app questions))))))))

;;; --- Extension ---------------------------------------------------------------

(defextension ask
  (:provides
   (tool ask
     :label "Ask"
     :description
     "Ask the user a clarifying question with multiple choices during a task. \
Use when multiple approaches have materially different tradeoffs the user must \
decide. Each question carries an id, question text, and 2-5 options (each a \
label plus optional description). Set \"recommended\" to the 0-based index of \
the default option; \"(Recommended)\" is shown beside it. Set \"multi\" true to \
allow multiple selections. \"Other (type your own)\" is always appended as the \
last choice; do not include it yourself. Choices are navigated with up/down; \
multiple questions are navigated with left/right. Pass \"questions\" as a JSON \
string."
     :parameters '(:object (:questions :string))
     :runner #'ask-run
     :metadata '())))
