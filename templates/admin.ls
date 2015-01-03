;;; -*- mode: lisp; -*-
"use strict"

;; A few shortcuts to save typing. This is not real macros because it
;; performs replacement only, doesn't run code, i.e. effectively
;; everything is inside a quasiquote. And the syntax is weird. And
;; since it doesn't support quoting or symbol-name or stringification,
;; the displayName will be attached at runtime...
(macro fn (args rest...)
       (function ~args ~rest...))
(macro defun (name args rest...)
       (var ~name (fn ~args ~rest...)))
(macro obj (rest...)
       (object ~rest...))
(macro defvar (name value) (var ~name ~value))

($
 (fn ()

   ;; Aliases to help minification and macros to save typing.
   (defvar React_createElement React.createElement)
   (defvar React_createClass React.createClass)
   (defvar React_PropTypes React.PropTypes)
   (defvar __map _.map)
   (macro e (name attrs rest...)
          (React_createElement ~name (obj ~@attrs) ~rest...))
   (macro defcomponent (name rest...)
          (var ~name (React_createClass (_.defaults (obj ~rest...)
                                                    (obj render (fn () false))
                                                    (_.invert (obj ~name "displayName"))))))
   (macro ifentity (attr)
          (if this.props.entity (~attr this.props.entity) ""))

   ;; Identity of current user through meta elements
   (defvar identUser (-> ($ "#meta-user") (.attr "value")))
   (defvar identPriv (-> ($ "#meta-priv") (.attr "value")))
   (macro whenadmin (thing) (if (= identPriv "PrivAdmin") ~thing null))
   (macro whennotadmin (thing) (if (= identPriv "PrivAdmin") null ~thing))

   ;; An APIConnection is a wrapper around WebSocket.
   (defun APIConnection (pathname)
     (defvar wsUrl
       (+ (+ (if (= window.location.protocol "https:") "wss://" "ws://")
             window.location.host)
          pathname))
     (defvar conn null)
     (defvar callback null)
     (defvar timeConnected null)
     (defun connect ()
       (if (> (- (Date.now) timeConnected) 2000)
         (do
           (set timeConnected (Date.now))
           (set conn (new WebSocket wsUrl))
           (set conn.onmessage callback)
           (set conn.onerror (fn ()
                               (console.log "WS connection errored.")
                               (connect)))
           (set conn.onclose (fn (e)
                               (console.log e)
                               (console.log "WS connection closed."))))
         (setTimeout connect (- (Date.now) timeConnected))))
     (-> ($ window) (.on "beforeunload" (fn () (conn.close))))
     (obj
      registerCallback (fn (func)
                         (if (! conn) (connect) (_.noop))
                         (defun wrapFunc (e) (func (JSON.parse e.data)))
                         (set conn.onmessage wrapFunc)
                         (set callback wrapFunc))
      pathname (fn () pathname)
      close (fn ()
              (set conn.onmessage _.noop)
              (set conn.onerror _.noop)
              (set conn.onclose _.noop)
              (conn.close))))

   ;; An EntityRow is a row of data in the table. Each row also has
   ;; two action buttons.
   (defcomponent EntityRow
     propTypes
     (obj
      firstRowSpan React_PropTypes.number.isRequired
      entity React_PropTypes.object.isRequired
      entityEditor React_PropTypes.any.isRequired
      auxiliary React_PropTypes.object)

     render
     (fn ()
       (defvar that this)
       (defvar dataSpec (.dataSpec (get window.location.pathname pageSpec)))
       (defvar firstCell (if (&& (! (null? dataSpec.categoryColumn))
                                 that.props.firstRowSpan)
                           (do
                             (defvar value (get (get 0 dataSpec.categoryColumn) that.props.entity))
                             (defvar mapper (get 1 dataSpec.categoryColumn))
                             (e "td" (rowSpan that.props.firstRowSpan)
                               (mapper.apply that.props.auxiliary (array value that.props.entity))))
                           null))
       (defun onEditButtonClick ()
         (React.render
          (e that.props.entityEditor (auxiliary that.props.auxiliary entity that.props.entity))
          (getModalWrapper)))
       (defun onDeleteButtonClick ()
         (React.render
          (e DeleteConfirmation (auxiliary that.props.auxiliary entity that.props.entity))
          (getModalWrapper)))
       (e "tr" ()
         firstCell
         (__map dataSpec.columns
                (fn (spec idx)
                  (defvar value (get (get 0 spec) that.props.entity))
                  (defvar mapper (get 1 spec))
                  (e "td" (key idx) (mapper.apply that.props.auxiliary (array value that.props.entity)))))
         (whenadmin
           (e "td" (className "text-right")
             (e "div" (className "btn-group" role "group")
               (e "button" (type "button" className "btn btn-default btn-xs" title "Edit" onClick onEditButtonClick)
                 (e "span" (className "glyphicon glyphicon-pencil" "aria-hidden" "true")))
               (e "button" (type "button" className "btn btn-default btn-xs" title "Delete" onClick onDeleteButtonClick)
                 (e "span" (className "glyphicon glyphicon-trash" "aria-hidden" "true")))))))))

   ;; An EntityCategory is a group of data shared under a category,
   ;; presented as a tbody with a single cell spanning all of them.
   (defcomponent EntityCategory
     propTypes
     (obj
      entities React_PropTypes.array.isRequired
      entityEditor React_PropTypes.any.isRequired
      auxiliary React_PropTypes.object)
     render
     (fn ()
       (defvar that this)
       (e "tbody" ()
         (__map this.props.entities
                (fn (entity i entities)
                  (e EntityRow
                      (key entity.id entityEditor that.props.entityEditor auxiliary that.props.auxiliary entity entity firstRowSpan (if i 0 entities.length))))))))

   ;; An EntityTable is the entire table for holding the data. It
   ;; receives events from the two action buttons on each row.
   (defcomponent EntityTable
     propTypes
     (obj
      conn React_PropTypes.object.isRequired
      entityEditor React_PropTypes.any.isRequired
      auxiliary React_PropTypes.object
      customFilter React_PropTypes.func)

     ;; Assume no data for initial state.
     getInitialState
     (fn () (obj tableData (obj data (array))))

     ;; When mounted, add callback and event handlers.
     componentDidMount
     (fn ()
       (defvar that this)
       (this.props.conn.registerCallback
        (fn (d)
          (that.setState (obj tableData d)))))

     ;; When updated, it is probably because a new connection is here;
     ;; re-register the callback. It doesn't hurt to register again.
     componentDidUpdate
     (fn ()
       (defvar that this)
       (this.props.conn.registerCallback
        (fn (d)
          (that.setState (obj tableData d)))))

     ;; Close connection as appropriate.
     componentWillUnmount
     (fn () (this.props.conn.close))
     componentWillReceiveProps
     (fn (newProps) (if (!= (this.props.conn.pathname) (newProps.conn.pathname)) (this.props.conn.close)))

     ;; Now we need to group the data according to the requirements
     ;; outlined in the dataSpec (a.k.a. massage) and then render
     ;; them. The massaging process has type [Object] -> [[Object]].
     ;; [[Object]].
     render
     (fn ()
       (defvar that this)
       (defvar dataSpec (.dataSpec (get window.location.pathname pageSpec)))
       (defvar rawData ((|| this.props.customFilter _.identity) this.state.tableData.data))
       (defvar rows (if (null? dataSpec.categoryColumn)
                      (do ; no category column, one row per entry
                          ; without using EntityCategory
                        (defvar sortName (get 0 (get 0 dataSpec.columns)))
                        (defvar massagedData (_.sortBy rawData sortName))
                        (e "tbody" ()
                          (__map massagedData
                                 (fn (entity)
                                   (e EntityRow (firstRowSpan 1 entityEditor that.props.entityEditor auxiliary that.props.auxiliary entity entity key entity.id))))))
                      (do ; there is a category column, so group by
                          ; that first
                        (defvar categoryName (get 0 dataSpec.categoryColumn))
                        (defvar sortName (get 0 (get 0 dataSpec.columns)))

                        ;; This is dense, so a reminder of the types:
                        ;; groupBy :: (Eq b, Show b) => [a] -> (a -> b) -> {[a]}
                        ;; map :: {v} -> (v -> String -> a) -> [a]
                        ;; sortBy :: [{a}] -> String -> [{a}]
                        (defvar massagedData
                          (__map (_.sortBy (__map (_.groupBy rawData categoryName)
                                                  (fn (v k) (obj k k v (_.sortBy v sortName))))
                                           "k")
                                 (fn (d) (.v d))))
                        (__map massagedData
                               (fn (entities)
                                 (defvar category (get categoryName (get 0 entities)))
                                 (e EntityCategory (entities entities entityEditor that.props.entityEditor auxiliary that.props.auxiliary key category)))))))

       (defvar headers (__map (-> (if (null? dataSpec.categoryColumn)
                                    (array)
                                    (array (get 2 dataSpec.categoryColumn)))
                                  (.concat (__map dataSpec.columns (fn (v) (get 2 v)))))
                              (fn (label idx) (e "th" (key idx) label))))
       (e "div" (className "table-responsive")
         (e "table" (className "table")
           (e "thead" ()
             (e "tr" ()
               headers
               (whenadmin (e "th" ()))))
           rows))))

   ;; The Modal dialog that takes control of input and needs to be
   ;; dealt with before the rest of the page is functional.
   (defcomponent Modal
     propTypes
     (obj
      canClose React_PropTypes.bool.isRequired
      title React_PropTypes.node.isRequired
      buttons React_PropTypes.node
      children React_PropTypes.node.isRequired)

     ;; Modal rendering.
     render
     (fn ()
       (defvar header
         (e "div" (className "modal-header")
           (if this.props.canClose
             (e "button" (type "button" className "close" id "modalClose" "data-dismiss" "modal")
               (e "span" ("aria-hidden" "true") "×")
               (e "span" (className "sr-only") "Close"))
             "")
           (e "h4" (className "modal-title") this.props.title)))
       (defvar footer
         (if this.props.buttons
           (e "div" (className "modal-footer") this.props.buttons)
           ""))
       (e "div" (id "modal" className "modal fade")
         (e "div" (className "modal-dialog")
           (e "div" (className "modal-content")
             header
             (e "div" (className "modal-body") this.props.children)
             footer))))

     ;; When it is mounted or updated, it needs to be shown.
     componentDidMount (fn ()
                         (-> ($ (this.getDOMNode))
                             (.on "hidden.bs.modal" (fn () (React.unmountComponentAtNode (getModalWrapper))))
                             (.modal (obj keyboard false backdrop "static")))))

   ;; The target for rendering the Modal.
   (defun getModalWrapper () (-> ($ "#modal-wrapper") (.get 0)))

   (defun dismissModal () (-> ($ "#modal") (.modal "hide")))

   ;; An action modal, with custom content, a Cancel button and an
   ;; action button.
   (defcomponent ActionModal
     propTypes
     (obj
      actionButtonType React_PropTypes.string
      actionButtonStyle React_PropTypes.string.isRequired
      actionButtonLabel React_PropTypes.node.isRequired
      title React_PropTypes.node.isRequired
      children React_PropTypes.node.isRequired
      next React_PropTypes.func.isRequired)

     getInitialState
     (fn () (obj spinner 0))
     render
     (fn ()
       (defvar that this)
       (defun onActionButtonClick (e)
         (e.preventDefault)
         (defun setSpinner (v) (that.setState (obj spinner v)))
         (that.props.next (fn () (dismissModal)) setSpinner))
       (defvar buttons
         (e "div" ()
           ;; Note that we must render both elements in both cases
           ;; because we don't want to deal with re-attaching event
           ;; handlers.
           (e "img" (width 16 height 16 src "/static/res/loading.gif" style (obj display (if this.state.spinner "inline" "none"))))
           (e "div" (style (obj display (if this.state.spinner "none" "block")))
             (e "button" (type "button" className "btn btn-default" "data-dismiss" "modal") "Cancel")
             (e "button" (type (|| this.props.actionButtonType "button") className (+ "btn btn-" this.props.actionButtonStyle) onClick onActionButtonClick) this.props.actionButtonLabel))))
       (e Modal (canClose true title this.props.title buttons buttons)
         this.props.children)))

   ;; An editor for records.
   (defcomponent RecordEditor
     propTypes
     (obj
      entityTypeHumanName React_PropTypes.string.isRequired
      entityTypeMachineName React_PropTypes.string.isRequired
      entity React_PropTypes.object
      children React_PropTypes.node.isRequired)

     getInitialState
     (fn ()
       (obj err null))

     render
     (fn ()
       (defvar that this)
       (defvar hname this.props.entityTypeHumanName)
       (defvar mname this.props.entityTypeMachineName)
       (defvar title (if this.props.entity (+ "Edit " hname) (+ "Add a new " hname)))
       (defvar actionButtonLabel (if this.props.entity "Edit" "Add"))
       (defvar endpoint (if this.props.entity
                          (+ (+ (+ "/api/" mname) "/") this.props.entity.id)
                          (+ "/api/" mname)))
       (defvar method (if this.props.entity "PUT" "POST"))
       (defun onError (jqxhr)
         (defvar resp (JSON.parse jqxhr.responseText))
         (that.setState (obj err resp.meta.details)))
       (defun next (hideModal setSpinner)
         (setSpinner 1)
         (console.log (-> ($ "#editorForm") (.serialize)))
         ($.ajax endpoint (obj type method
                               data (-> ($ "#editorForm") (.serialize))
                               success hideModal
                               error (fn (jqxhr)
                                       (setSpinner 0)
                                       (console.log "http error")
                                       (console.log jqxhr)
                                       (onError jqxhr)))))
       (e "form" (id "editorForm" role "form")
         (e ActionModal (title title actionButtonLabel actionButtonLabel actionButtonStyle "primary" actionButtonType "submit" next next)
           (if this.state.err
             (e "div" (className "alert alert-danger" role "alert") this.state.err)
             null)
           this.props.children))))

   ;; The CCA editor control.
   (defcomponent CcaEditor
     propTypes
     (obj
      entity React_PropTypes.object)

     render
     (fn ()
       (e RecordEditor (entity this.props.entity entityTypeHumanName "CCA" entityTypeMachineName "ccas")
         (e "div" (className "form-group")
           (e "label" (htmlFor "name") "CCA Name")
           (e "input" (type "text" className "form-control" name "name" placeholder "e.g. Infocomm Club" defaultValue (ifentity .name))))
         (e "div" (className "form-group")
           (e "label" (htmlFor "category") "CCA Category")
           (e "input" (type "text" className "form-control" name "category" placeholder "e.g. Clubs and Societies" defaultValue (ifentity .category)))))))

   (defcomponent SubjectEditor
     propTypes
     (obj
      entity React_PropTypes.object)

     getInitialState
     (fn ()
       (obj compulsory (if (! this.props.entity) false (null? this.props.entity.code))))

     render
     (fn ()
       (var that this)
       (defun onCompulsoryChanged (event)
         (that.setState (obj compulsory event.target.checked)))
       (e RecordEditor (entity this.props.entity entityTypeHumanName "Subject" entityTypeMachineName "subjects")
         (e "div" (className "form-group")
           (e "label" (htmlFor "name") "Subject Name")
           (e "input" (type "text" className "form-control" name "name" placeholder "e.g. Mathematics (H3)" defaultValue (ifentity .name)))
           (e "div" (className "checkbox")
             (e "label" ()
               (e "input" (type "checkbox" onChange onCompulsoryChanged checked this.state.compulsory)) "This is a compulsory subject."))
           (e "div" (className "checkbox")
             (e "label" ()
               (e "input" (type "checkbox" name "science" defaultChecked (if this.props.entity this.props.entity.is_science false))) "This is a science subject.")))
         (e "div" (className "form-group")
           (e "label" (htmlFor "code") "Subject Code")
           (if this.state.compulsory
             (e "input" (type "text" className "form-control" disabled true value "" placeholder "None"))
             (e "input" (type "text" className "form-control" name "code" placeholder "e.g. MA(H3)" defaultValue (ifentity .code))))
          (e "p" (className "help-block") "Compulsory subjects do not have a subject code, because since everyone takes them, there is no reason to specify them in CSV. They will, however, still appear on PDF files if they are also science subjects."))
         (e "div" (className "form-group")
           (e "label" (htmlFor "level") "Applies To")
           (e "div" (className "checkbox")
             (__map (array 1 2 3 4 5 6)
                    (fn (lv)
                      (defvar checked (if that.props.entity (!= -1 (_.indexOf that.props.entity.level lv)) false))
                      (e "label" (key lv className "checkbox-inline")
                        (e "input" (type "checkbox" name "level" value lv defaultChecked checked)) "Year " lv)))))
         (e "div" (className "checkbox")
           (e "label" ()
             (e "input" (type "checkbox" name "force" defaultChecked false)) "Force the operation to continue despite errors (not recommended).")))))

   (defcomponent BatchUploadStudents
     getInitialState
     (fn () (obj err null))
     render
     (fn ()
       (defvar that this)
       (defun next (hideModal setSpinner)
         (setSpinner 1)
         (defvar formData (new FormData (-> ($ "#uploaderForm") (.get 0))))
         (console.log formData)
         (defun onError (jqxhr)
           (defvar resp (JSON.parse jqxhr.responseText))
           (that.setState (obj err resp.meta.details)))
         ($.ajax "/api/students/many"
                 (obj
                  type "POST"
                  data formData
                  contentType false
                  processData false
                  success hideModal
                  error (fn (jqxhr)
                          (setSpinner 0)
                          (console.log "http error")
                          (console.log jqxhr)
                          (onError jqxhr)))))
       (e ActionModal (actionButtonStyle "primary" actionButtonLabel "Upload" title "Add Students via Uploading CSV File" next next)
         (if this.state.err
           (e "div" (className "alert alert-danger" role "alert") this.state.err)
           null)
         (e "form" (id "uploaderForm" role "form")
           (e "p" (className "help-block") "This allows you to upload a CSV file of students and add all of them. This is an all-or-nothing operation: even if only one student could not be added, none of the students will be added.")
           (e "div" (className "form-group")
             (e "label" (htmlFor "csv") "CSV File")
             (e "input" (type "file" className "form-control" name "csv" accept "text/csv,.csv" required true)))))))

   (defcomponent TestDecoder
     getInitialState
     (fn () (obj decodeResult null))
     render
     (fn ()
       (defvar that this)
       (defun next (hideModal setSpinner)
         (setSpinner 1)
         ($.getJSON "/api/subjects/test-decode"
                    (-> ($ "#decoderForm") (.serialize))
                    (fn (data)
                      (setSpinner 0)
                      (that.setState (obj decodeResult data.data)))))
       (defun subjectsToString (ss) (-> (__map ss (fn (s) s.name)) (.join ", ")))
       (e ActionModal (actionButtonStyle "primary" actionButtonLabel "Decode" title "Test Decode Subject Codes" next next)
         (if this.state.decodeResult
           (e "div" (className "panel panel-default")
             (e "div" (className "panel-heading") "Decode Results")
             (e "div" (className "panel-body")
               (cond (= 0 this.state.decodeResult.length)
                     "The subject codes could not be decoded at all."
                     (= 1 this.state.decodeResult.length)
                     (e "div" ()
                       "The subject codes could be unambiguously decoded: "
                       (e "br" ())
                       (subjectsToString (get 0 this.state.decodeResult)))
                     true
                     (e "div" ()
                       "The subject codes could not be unambiguously decoded; here are the possibilities:"
                       (e "ul" ()
                         (__map this.state.decodeResult
                                (fn (ss) (e "li" () (subjectsToString ss)))))))))
           null)
         (e "form" (id "decoderForm" role "form")
           (e "p" (className "help-block") "This decoder allows you to preview decoding of a set of subject codes. You can enter a series of subject codes and see how it will be decoded.")
           (e "div" (className "form-group")
             (e "label" (htmlFor "level") "Year")
             (e "div" (className "radio")
               (__map (array 1 2 3 4 5 6)
                      (fn (lv)
                        (e "label" (key lv className "checkbox-inline")
                          (e "input" (type "radio" name "level" value lv defaultChecked (= 1 lv))) "Year " lv)))))
           (e "div" (className "form-group")
             (e "label" (htmlFor "str") "Subject Code Combination")
             (e "input" (type "text" className "form-control" name "str" placeholder "Enter subject codes here")))))))

   (defcomponent TeacherEditor
     propTypes
     (obj
      entity React_PropTypes.object)

     render
     (fn ()
       (e RecordEditor (entity this.props.entity entityTypeHumanName "Teacher" entityTypeMachineName "teachers")
         (e "div" (className "form-group")
           (e "label" (htmlFor "name") "Teacher Name")
           (e "input" (type "text" className "form-control" name "name" placeholder "e.g. Chow Ban Hoe" defaultValue (ifentity .name)))
           (e "div" (className "checkbox")
             (e "label" ()
               (e "input" (type "checkbox" name "admin" defaultChecked (if this.props.entity this.props.entity.is_admin false))) "This teacher is an administrator.")))
         (e "div" (className "form-group")
           (e "label" (htmlFor "witness") "Witness Name (Capital, with Salutation)")
           (e "input" (type "text" className "form-control" name "witness" placeholder "e.g. MR CHOW BAN HOE" defaultValue (ifentity .witness_name))))
         (e "div" (className "form-group")
           (e "label" (htmlFor "email") "Email Address")
           (e "input" (type "email" className "form-control" name "email" placeholder "e.g. chow_ban_hoe@moe.edu.sg" defaultValue (ifentity .email))))
         (e "div" (className "form-group")
           (e "label" (htmlFor "unit") "Unit")
           (e "input" (type "email" className "form-control" name "unit" placeholder "e.g. Bio" defaultValue (ifentity .unit)))))))

   (defcomponent StudentEditor
     propTypes
     (obj
      auxiliary React_PropTypes.object.isRequired
      entity React_PropTypes.object)

     getInitialState
     (fn ()
       (obj currentLevel (if this.props.entity (get 0 (get "class" this.props.entity)))))

     render
     (fn ()
       (defvar that this)
       (defun classChange (e)
         (defvar match (-> /^([1-6])[A-NP-Z]$/ (.exec e.target.value)))
         (that.setState (obj currentLevel (if match (parseInt (get 1 match) 10) null))))
       (e RecordEditor (entity this.props.entity entityTypeHumanName "Student" entityTypeMachineName "students")
         (e "div" (className "form-group")
           (e "label" (htmlFor "class") "Class")
           (e "input" (type "text" className "form-control" name "class" placeholder "e.g. 5N" defaultValue (if this.props.entity (-> (get "class" this.props.entity) (.join "")) "") onChange classChange)))
         (e "div" (className "form-group")
           (e "label" (htmlFor "indexno") "Register Number")
           (e "input" (type "number" className "form-control" name "indexno" placeholder "e.g. 22" defaultValue (ifentity .index_number))))
         (e "div" (className "form-group")
           (e "label" (htmlFor "name") "Full Name")
           (e "input" (type "text" className "form-control" name "name" inputmode "latin-name" defaultValue (ifentity .name))))
         (e "div" (className "form-group")
           (e "label" (htmlFor "chinesename") "Chinese Name")
           (e "input" (type "text" className "form-control" name "chinesename" inputmode "kana" defaultValue (ifentity .chinese_name))))
         (e "div" (className "form-group")
           (e "label" (htmlFor "nric") "Partial NRIC")
           (e "input" (type "text" className "form-control" name "nric" inputmode "verbatim" defaultValue (ifentity .nric))))
         (e "div" (className "form-group")
           (e "label" (htmlFor "witnesser") "Witness")
           (e "select" (className "form-control" name "witness" defaultValue (ifentity .witnesser))
             (-> (array (e "option" (key 0 value "") "None"))
                 (.concat
                  (__map this.props.auxiliary.teacherInfo.data
                         (fn (teacher)
                           (e "option" (value teacher.id key teacher.id) teacher.name " (" teacher.witness_name ")")))))))
         (e "div" (className "form-group")
           (e "label" (htmlFor "subj") "Subject Combination")
           (e "div" (className "checkbox")
             (if this.state.currentLevel
               (__map (_.filter that.props.auxiliary.subjectInfo.data
                                (fn (subject) (&& (_.contains subject.level that.state.currentLevel)
                                                  (! (null? subject.code)))))
                      (fn (subject idx)
                        (e "label" (className "checkbox-inline" key subject.id)
                          (e "input" (type "checkbox" name "subj" value (+ 1 idx)
                                           defaultChecked (if that.props.entity (_.contains that.props.entity.subject_combi subject.id) false)))
                          subject.name " (" subject.code ")")))
               (e "p" (className "help-block") "Subjects are not available for selection because you did not correctly enter a class.")))))))

   ;; The Delete Confirmation.
   (defcomponent DeleteConfirmation
     propTypes
     (obj
      entity React_PropTypes.object.isRequired)

     render
     (fn ()
       (defvar dataSpec (.dataSpec (get window.location.pathname pageSpec)))
       (defvar hname dataSpec.humanName)
       (defvar mname dataSpec.machineName)
       (defvar endpoint (+ (+ (+ "/api/" mname) "/") this.props.entity.id))
       (defun next (hideModal)
         ($.ajax endpoint (obj type "DELETE" complete hideModal)))
       (defvar message (+ (+ (+ (+ "Are you sure you want to delete the " hname) " “") this.props.entity.name)
                          "” from the database?"))
       (e ActionModal
           (title (+ "Delete " hname) actionButtonLabel "Delete" actionButtonStyle "danger" next next)
         (e "p" () message))))

   ;; The Admin console homepage.
   (defcomponent AdminHomeR
     render
     (fn ()
       (e "div" (className "row")
         (e "div" (className "col-sm-11 col-md-8 col-lg-7")
           (e "h2" () "Welcome")
           (e "p" () "Welcome to the admin console for RVHS Science Lab Undertaking Project. Click Manage Students from the tab above to view information about students." (whenadmin " As an administrator, you can also manage other things from the above tabs."))))))

   (defcomponent EntityPage
     propTypes
     (obj
      customButtons React_PropTypes.node
      wsUrl React_PropTypes.string
      auxiliary React_PropTypes.object
      customFilter React_PropTypes.func)

     getInitialState
     (fn ()
       (defvar dataSpec (.dataSpec (get window.location.pathname pageSpec)))
       (defvar mname dataSpec.machineName)
       (obj
        conn (APIConnection (|| this.props.wsUrl (+ "/api/" mname)))))

     componentWillReceiveProps
     (fn (newProps)
       (defvar that this)
       (if (!= newProps.wsUrl this.props.wsUrl)
         (do
           (that.state.conn.close)
           (that.setState (obj conn (APIConnection newProps.wsUrl))))))

     render
     (fn ()
       (defvar that this)
       (defvar dataSpec (.dataSpec (get window.location.pathname pageSpec)))
       (defvar hnamepl dataSpec.humanNamePlural)
       (defvar mname dataSpec.machineName)
       (defvar editor dataSpec.editor)
       (defun onAddButtonClick ()
         (React.render
          (e editor (auxiliary that.props.auxiliary))
          (getModalWrapper)))
       (defun onRemoveAllButtonClick ()
         (React.render
          (e ActionModal
              (title (+ "Deleting All " hnamepl) actionButtonLabel "Yes, Delete All" actionButtonStyle "danger" next (fn (hideModal) ($.ajax (+ "/api/" mname) (obj type "DELETE" complete hideModal))))
            (e "p" () (+ (+ (+ (+ "Are you sure you want to delete all " hnamepl) " currently stored in the database? This will also delete all references to these ") hnamepl) ", if they exist.")))
          (getModalWrapper)))
       (e "div" ()
         (whenadmin
           (e "div" (className "pull-right btn-group" role "toolbar" "aria-label" "Action Buttons")
             (if this.props.customButtons this.props.customButtons null)
             (e "button" (type "button" className "btn btn-default" onClick onAddButtonClick) "Add New")
             (e "button" (type "button" className "btn btn-default" onClick onRemoveAllButtonClick) "Remove All")))
         (e "h2" () (+ "View " hnamepl))
         this.props.children
         (e EntityTable (conn this.state.conn entityEditor editor auxiliary this.props.auxiliary customFilter this.props.customFilter))))

     ;; Prevent establishing a new connection every time this is
     ;; rerendered.
     shouldComponentUpdate
     (fn (newProps)
       (! (_.isEqual this.props newProps))))

   ;; The admin console CCA page.
   (defcomponent AdminCcasR
     render
     (fn () (e EntityPage ())))

   (defcomponent AdminSubjectsR
     render
     (fn ()
       (defun onTestDecodeButtonClick ()
         (React.render
          (e TestDecoder ())
          (getModalWrapper)))
       (defvar customButtons (e "button" (onClick onTestDecodeButtonClick type "button" className "btn btn-default") "Test Decode"))
       (e EntityPage (customButtons customButtons))))

   (defcomponent AdminTeachersR
     render
     (fn () (e EntityPage ())))

   (defcomponent AdminStudentsR
     getInitialState
     (fn ()
       (defvar that this)
       (defvar ccaConn (APIConnection "/api/ccas"))
       (defvar teacherConn (APIConnection "/api/teachers"))
       (defvar subjectConn (APIConnection "/api/subjects"))
       (defvar classConn (APIConnection "/api/classes"))
       ;; TODO what if the callback sets the state before this function returns?
       (ccaConn.registerCallback (fn (d) (that.setState (obj ccaInfo d))))
       (teacherConn.registerCallback (fn (d) (that.setState (obj teacherInfo d))))
       (subjectConn.registerCallback (fn (d) (that.setState (obj subjectInfo d))))
       (classConn.registerCallback (fn (d) (that.setState (obj classInfo d))))
       (defvar emptyData (obj data (array)))
       (obj queryString "searchby=none"
            selected "class"
            hideWithoutWitness true
            ccaConn ccaConn
            teacherConn teacherConn
            subjectConn subjectConn
            ccaInfo emptyData
            teacherInfo emptyData
            subjectInfo emptyData
            classInfo emptyData))

     componentWillUnmount
     (fn ()
       (this.state.ccaConn.close)
       (this.state.teacherConn.close)
       (this.state.subjectConn.close))

     render
     (fn ()
       (defun onBatchUploadButtonClick ()
         (React.render
          (e BatchUploadStudents ())
          (getModalWrapper)))
       (defvar customButtons (e "button" (onClick onBatchUploadButtonClick type "button" className "btn btn-default") "Add New (Upload CSV File)"))
       (defvar that this)
       (defun onRadioChange (e)
         (if e.target.checked
           (that.setState (obj selected e.target.value))))
       (defun onCheckClick (e)
         (console.log e.target.checked)
         (that.setState (obj hideWithoutWitness e.target.checked)))
       (defun customFilter (data)
         (if that.state.hideWithoutWitness
           (_.filter data (fn (d) d.witnesser))
           data))
       (defun onViewButtonClick (e)
         (e.preventDefault)
         (that.setState (obj queryString (-> ($ "#searchbyForm") (.serialize)))))
       (defvar auxiliary
         (obj teacherInfo that.state.teacherInfo
              ccaInfo that.state.ccaInfo
              subjectInfo that.state.subjectInfo
              classInfo that.state.classInfo))
       (macro radiodetail (radioValue radioChecked helpText innerDeactivated innerActivated)
              (e "div" (className "radio form-group")
                (e "label" (className "col-sm-3 col-md-2 col-lg-2 control-label")
                  (e "input" (type "radio" name "searchby" value ~radioValue defaultChecked ~radioChecked onChange onRadioChange))
                  ~helpText)
                (e "div" (className "col-sm-9 col-md-7 col-lg-6")
                  (if (= this.state.selected ~radioValue) ~innerActivated ~innerDeactivated))))
       (defvar offsetClassName "col-sm-9 col-sm-offset-3 col-md-7 col-md-offset-2 col-lg-6 col-lg-offset-2")
       (e "div" ()
         (e "div" (className "row")
           (e "h4" (className offsetClassName) "Which students would you like to see?"))
         (e "form" (role "form" id "searchbyForm" className "form-horizontal")
           (radiodetail "class" true "Search By Class" (e "select" (className "form-control" disabled true))
                        (e "select" (className "form-control" name "class")
                          (__map auxiliary.classInfo.data
                                 (fn (klass)
                                   (defvar klassstr (+ (get 0 klass) (get 1 klass)))
                                   (e "option" (value klassstr key klassstr) klassstr)))))
           (radiodetail "level" false "Search By Level" (e "select" (className "form-control" disabled true))
                        (e "select" (className "form-control" name "level")
                          (__map (_.uniq auxiliary.classInfo.data false (fn (k) (get 0 k)))
                                 (fn (klass)
                                   (e "option" (value (get 0 klass) key (get 0 klass)) "Year " (get 0 klass))))))
           (radiodetail "name" false "Search By Approx. Name" (e "input" (type "text" className "form-control" disabled true))
                        (e "input" (type "text" className "form-control" name "name" placeholder "Enter an approximate name, e.g. Xin Yi" value "")))
           (radiodetail "subject" false "Search By Subject" (e "select" (className "form-control" disabled true))
                        (e "select" (className "form-control" name "id")
                          (__map auxiliary.subjectInfo.data
                                 (fn (subject)
                                   (e "option" (value subject.id key subject.id)
                                     subject.name " (" (-> (__map subject.level (fn (l) (+ "Year " l))) (.join ", ")) ")")))))
           (radiodetail "cca" false "Search By CCA" (e "select" (className "form-control" disabled true))
                        (e "select" (className "form-control" name "id")
                          (__map auxiliary.ccaInfo.data
                                 (fn (cca)
                                   (e "option" (value cca.id key cca.id) cca.name " (" cca.category ")")))))
           (radiodetail "teacher" false "Search By Witness" (e "select" (className "form-control" disabled true))
                        (e "select" (className "form-control" name "id")
                          (__map auxiliary.teacherInfo.data
                                 (fn (teacher)
                                   (e "option" (value teacher.id key teacher.id) teacher.name " (" teacher.witness_name ")")))))
           (e "div" (className "form-group")
             (e "div" (className offsetClassName)
               (e "div" (className "checkbox")
                 (e "label" ()
                   (e "input" (type "checkbox" defaultChecked true onChange onCheckClick) " Hide Students Without Witness")))))
           (e "div" (className "form-group")
             (e "div" (className offsetClassName)
               (e "button" (type "submit" className "btn btn-primary" onClick onViewButtonClick) "View"))))
         (e "div" (className "row")
           (e EntityPage (wsUrl (+ "/api/students?" this.state.queryString) auxiliary auxiliary customButtons customButtons customFilter customFilter))))))

   (defun lookupForeign (dataset id)
     (|| (_.find dataset.data (fn (v) (= id v.id))) "??"))

   (defcomponent SubmissionCompleteModal
     propTypes
     (obj
      sub React_PropTypes.object.isRequired
      ccaInfo React_PropTypes.object.isRequired
      onLockClick React_PropTypes.func.isRequired
      onUnlockClick React_PropTypes.func.isRequired)
     render
     (fn ()
       (defvar that this)
       (defvar button
         (e "button" (type "button" className "btn btn-default" "data-dismiss" "modal") "OK"))
       (defun onLockClick () (that.props.onLockClick) (dismissModal))
       (defun onUnlockClick () (that.props.onUnlockClick) (dismissModal))
       (e Modal (canClose true title "Student-Submitted Information" buttons button)
         (e "table" (className "table")
           (e "tbody" ()
             (e "tr" ()
               (e "th" () "Email") (e "td" () this.props.sub.email))
             (e "tr" ()
               (e "th" () "Phone Number") (e "td" () this.props.sub.phone))
             (e "tr" ()
               (e "th" () "Date of Submission") (e "td" () this.props.sub.date))
             (e "tr" ()
               (e "th" () "CCAs") (e "td" () (|| (-> (__map this.props.sub.cca (fn (s) (.name (lookupForeign that.props.ccaInfo s)))) (.join ", ")) "None")))
             (e "tr" ()
               (e "th" () "Delete Submission")
               (e "td" ()
                 (e "div" (className "btn-toolbar" role "toolbar")
                   (e "button" (type "button" className "btn btn-default" onClick onUnlockClick) "Delete and Unlock")
                   (e "button" (type "button" className "btn btn-default" onClick onLockClick) "Delete and Lock")))))))))

   (defvar pageSpec
     (obj "/admin"
          (obj pageName "Home"
               onlyAdmin false
               component AdminHomeR
               dataSpec null)

          "/admin/ccas"
          (obj pageName "Manage CCAs"
               onlyAdmin true
               component AdminCcasR
               dataSpec
               (obj humanName "CCA"
                    humanNamePlural "CCAs"
                    machineName "ccas"
                    editor CcaEditor
                    categoryColumn (array "category" _.identity "CCA Category")
                    columns (array (array "name" _.identity "CCA Name"))))

          "/admin/subjects"
          (obj pageName "Manage Subjects"
               onlyAdmin true
               component AdminSubjectsR
               dataSpec
               (obj humanName "Subject"
                    humanNamePlural "Subjects"
                    machineName "subjects"
                    editor SubjectEditor
                    categoryColumn (array "level" (fn (ls) (-> (__map ls (fn (l) (+ "Year " l))) (.join ", "))) "Applies To")
                    columns
                    (array
                     (array "name" _.identity "Subject Name")
                     (array "code" (fn (v) (if v (e "code" () v) (e "i" () "(None; Compulsory Subject)"))) "Subject Code")
                     (array "is_science" (fn (b) (if b "Yes" "No")) "Science Subject?"))))

          "/admin/teachers"
          (obj pageName "Manage Teachers"
               onlyAdmin true
               component AdminTeachersR
               dataSpec
               (obj humanName "Teacher"
                    humanNamePlural "Teachers"
                    machineName "teachers"
                    editor TeacherEditor
                    categoryColumn null
                    columns
                    (array
                     (array "name" _.identity "Name")
                     (array "unit" _.identity "Unit")
                     (array "email" _.identity "Email Address")
                     (array "is_admin" (fn (b) (if b "Yes" "No")) "Administrator?")
                     (array "witness_name" _.identity "Witness Name"))))

          "/admin/students"
          (obj pageName "Manage Students"
               onlyAdmin false
               component AdminStudentsR
               dataSpec
               (obj humanName "Student"
                    humanNamePlural "Students"
                    machineName "students"
                    editor StudentEditor
                    categoryColumn (array "class" _.identity "Class")
                    columns
                    (array
                     (array "index_number" _.identity "Index #")
                     (array "name" _.identity "Name")
                     (array "chinese_name" _.identity "Chinese")
                     (array "nric" (fn (v) (-> v (.slice -5))) "ID")
                     (array "subject_combi"
                            (fn (ss)
                              (defvar that this)
                              (defvar codes (-> (__map ss (fn (s) (.code (lookupForeign that.subjectInfo s)))) (.join ", ")))
                              (defvar names (-> (__map ss (fn (s) (.name (lookupForeign that.subjectInfo s)))) (.join ", ")))
                              (if codes (e "span" (title names) codes) "—"))
                            "Subject(s)")
                     (array "witnesser"
                            (fn (tid)
                              (if (null? tid) (e "i" () "None") (.name (lookupForeign this.teacherInfo tid))))
                            "Witness")
                     (array "submission" (fn (sub) (|| sub.email "—")) "Email")
                     (array "submission" (fn (sub) (|| sub.phone "—")) "Phone")
                     (array "submission"
                            (fn (sub)
                              (cond sub.final_declaration_filename
                                    (e "a" (className "btn btn-default btn-xs" target "_blank" href (+ "https://rvhs-sci-lab-undertaking.appspot.com/storage?filename=" (encodeURIComponent sub.final_declaration_filename)))
                                      (e "span" (className "glyphicon glyphicon-floppy-save" "aria-hidden" "true")))
                                    (= sub.tag "SubmissionCompleted")
                                    (e "img" (src "/static/res/loading.gif" width 16 height 16 style (obj margin "0 4px")))))
                            "PDF")
                     (array "submission"
                            (fn (sub entity)
                              (defvar that this)
                              (defvar lockicon (e "span" (className "glyphicon glyphicon-lock" "aria-hidden" "true")))
                              (defvar completeicon (e "span" (className "glyphicon glyphicon-ok" "aria-hidden" "true")))
                              (defun onUnlockClick ()
                                ($.ajax (+ (+ "/api/students/" entity.id) "/unlock") (obj type "POST")))
                              (defun onLockClick ()
                                ($.ajax (+ (+ "/api/students/" entity.id) "/lock") (obj type "POST")))
                              (defun onCompleteClick ()
                                (React.render
                                 (e SubmissionCompleteModal (ccaInfo that.ccaInfo sub sub onLockClick onLockClick onUnlockClick onUnlockClick))
                                 (getModalWrapper)))
                              (e "div" (className "btn-group" role "toolbar" "aria-label" "Status Buttons")
                                (cond (= sub.tag "SubmissionNotOpen")
                                      (e "button" (type "button" className "btn btn-danger btn-xs" onClick onUnlockClick title "Click to unlock submission.") lockicon (e "span" (className "presentation-text" "data-text" " Locked")))
                                      (= sub.tag "SubmissionOpen")
                                      (e "button" (type "button" className "btn btn-primary btn-xs" onClick onLockClick title "Click to lock submission.") lockicon (e "span" (className "presentation-text" "data-text" " Unlocked")))
                                      (= sub.tag "SubmissionCompleted")
                                      (e "button" (type "button" className "btn btn-success btn-xs" onClick onCompleteClick title "Click to view submitted information.") completeicon (e "span" (className "presentation-text" "data-text" " Completed"))))))
                            "Status"))))))

   (defcomponent Page
     render
     (fn ()
       (defvar pathname window.location.pathname)
       (defvar tabs
         (__map pageSpec
                (fn (page route)
                  (defvar tab (e "li" (key route role "presentation" className (if (= route pathname) "active" ""))
                                (e "a" (href (if (= route pathname) "#" route))
                                  (.pageName page))))
                  (if (.onlyAdmin page) (whenadmin tab) tab))))
       (e "div" (id "content-wrapper")
         (e "div" (id "popover-wrapper"))
         (e "div" (id "modal-wrapper"))
         (e "div" (className "container")
           (e "div" (className "page-header")
             (e "h1" ()
               (e "img" (src "/static/res/rv.png" style (obj height "1em" position "relative" top "-0.2em" margin "0 0.3em 0 0")))
               "RVHS Science Lab Undertaking — For Teachers"))
           (e "p" ()
             (+ (+ "You are logged in as " identUser) ". ")
             (whenadmin "You are an administrator. ")
             (e "a" (href "/auth/logout") "Click here to logout. "))
           (e "div" (role "tabpanel")
             (e "ul" (className "nav nav-tabs")
               tabs))
           (e "div" (id "main-content")))))
     componentDidMount
     (fn ()
       (defvar pathname window.location.pathname)
       (if (undefined? window.WebSocket)
         (React.render
          (e Modal (canClose false title "Browser Unsupported")
            (e "p" ()
              "Your browser is too old to use this website. This website requires at least Internet Explorer version 10, Apple Safari version 7, Google Chrome version 16, or Mozilla Firefox version 11. Regardless of which broswer you are using, it is always recommended that you use the latest version available."))
          (getModalWrapper))
         (React.render
          (e (.component (get pathname pageSpec)) ())
          (-> ($ "#main-content") (.get 0))))))

   (React.render (e Page ()) document.body)))


;;; Local variables:
;;; eval: (put 'fn 'lisp-indent-function 'defun)
;;; eval: (put 'e 'lisp-indent-function 2)
;;; eval: (put 'if 'lisp-indent-function 1)
;;; eval: (put 'do 'lisp-indent-function 0)
;;; eval: (add-hook 'after-save-hook (lambda () (shell-command "lispy admin.ls") (shell-command "/Library/Internet\\ Plug-Ins/JavaAppletPlugin.plugin/Contents/Home/bin/java -jar ../../scratch/closure-compiler/compiler.jar --compilation_level SIMPLE_OPTIMIZATIONS --language_in ECMASCRIPT5_STRICT --js admin.js > ../static/admin.min.js")) nil t)
;;; End:
