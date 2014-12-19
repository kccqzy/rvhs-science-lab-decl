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
   (macro e (name attrs rest...)
          (React_createElement ~name (obj ~@attrs) ~rest...))
   (macro defcomponent (name rest...)
          (var ~name (React_createClass (_.defaults (obj ~rest...)
                                                    (obj render (fn () false))
                                                    (_.invert (obj ~name "displayName"))))))

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
           (set conn (new WebSocket wsUrl))
           (set conn.onopen (fn () (set timeConnected (Date.now))))
           (set conn.onmessage callback)
           (set conn.onerror (fn ()
                               (console.log "WS connection errored; how?")
                               (connect)))
           (set conn.onclose (fn ()
                               (console.log "WS connection closed; why?"))))
         (setTimeout connect (- (Date.now) timeConnected))))
     (connect)
     (-> ($ window) (.on "beforeunload" (fn () (conn.close))))
     (obj
      registerCallback (fn (func)
                         (set conn.onmessage func)
                         (set callback func))
      readyState (fn ()
                   (if conn conn.readyState -1))
      close (fn () (conn.close))))

   ;; An EntityRow is a row of data in the table. Each row also has
   ;; two action buttons.
   (defcomponent EntityRow
     propTypes
     (obj
      firstRowSpan React.PropTypes.number.isRequired
      entity React.PropTypes.object.isRequired)

     render
     (fn ()
       (defvar that this)
       (defvar dataSpec (.dataSpec (get window.location.pathname pageSpec)))
       (defvar firstCell (if (&& (! (null? dataSpec.categoryColumn))
                                 this.props.firstRowSpan)
                           (e "td" (rowSpan this.props.firstRowSpan)
                             ((get 1 dataSpec.categoryColumn) (get (get 0 dataSpec.categoryColumn) this.props.entity)))
                           null))
       (e "tr" ()
         firstCell
         (_.map dataSpec.columns
                (fn (spec idx)
                  (defvar value (get (get 0 spec) that.props.entity))
                  (defvar mapper (get 1 spec))
                  (e "td" (key idx) (mapper value))))
         (e "td" (className "text-right")
           (e "div" (className "btn-group" role "group" "aria-label" "Action Buttons")
             (e "button" (type "button" className "btn btn-default btn-xs" "aria-label" "Edit" "data-action" "edit" "data-entityid" this.props.entity.id)
               (e "span" (className "glyphicon glyphicon-pencil" "aria-hidden" "true")))
             (e "button" (type "button" className "btn btn-default btn-xs" "aria-label" "Delete" "data-action" "delete" "data-entityid" this.props.entity.id)
               (e "span" (className "glyphicon glyphicon-trash" "aria-hidden" "true"))))))))

   ;; An EntityCategory is a group of data shared under a category,
   ;; presented as a tbody with a single cell spanning all of them.
   (defcomponent EntityCategory
     propTypes
     (obj
      entities React.PropTypes.array.isRequired)
     render
     (fn ()
       (e "tbody" ()
         (_.map this.props.entities
                (fn (entity i entities)
                  (e EntityRow
                      (key entity.id entity entity firstRowSpan (if i 0 entities.length))))))))

   ;; An EntityTable is the entire table for holding the data. It
   ;; receives events from the two action buttons on each row.
   (defcomponent EntityTable
     propTypes
     (obj
      conn React.PropTypes.object.isRequired
      entityEditor React.PropTypes.any.isRequired  ; we actually expect a React class here
      deleteConfirmation React.PropTypes.any.isRequired)

     ;; Assume no data for initial state.
     getInitialState
     (fn () (obj tableData (obj data (array))))

     ;; When mounted, add event handlers.
     componentDidMount
     (fn ()
       (defvar that this)
       (this.props.conn.registerCallback
        (fn (e)
          (that.setState (obj tableData (JSON.parse e.data)))))
       (-> ($ (this.getDOMNode))
           (.on "click" "button[data-action=\"edit\"]"
                (fn ()
                  (defvar entityid (-> ($ this) (.data "entityid")))
                  (defvar entity (_.find that.state.tableData.data (fn (d) (= d.id entityid))))
                  (React.render
                   (e that.props.entityEditor (entity entity))
                   (getModalWrapper))))
           (.on "click" "button[data-action=\"delete\"]"
                (fn ()
                  (defvar entityid (-> ($ this) (.data "entityid")))
                  (defvar entity (_.find that.state.tableData.data (fn (d) (= d.id entityid))))
                  (React.render
                   (e that.props.deleteConfirmation (entity entity))
                   (getModalWrapper))))))

     ;; Close connection when unmounted. This is paranoia because
     ;; currently there is no event to unmount them.
     componentWillUnmount
     (fn () (this.props.conn.close))

     ;; Now we need to group the data according to the requirements
     ;; outlined in the dataSpec (a.k.a. massage) and then render
     ;; them. The massaging process has type [Object] -> [[Object]].
     ;; [[Object]].
     render
     (fn ()
       (defvar dataSpec (.dataSpec (get window.location.pathname pageSpec)))
       (defvar rawData this.state.tableData.data)
       (defvar rows (if (null? dataSpec.categoryColumn)
                      (do ; no category column, one row per entry
                          ; without using EntityCategory
                        (defvar sortName (get 0 (get 0 dataSpec.columns)))
                        (defvar massagedData (_.sortBy rawData sortName))
                        (e "tbody" ()
                          (_.map massagedData
                                 (fn (entity)
                                   (e EntityRow (firstRowSpan 1 entity entity key entity.id))))))
                      (do ; there is a category column, so group by
                          ; that first
                        (defvar categoryName (get 0 dataSpec.categoryColumn))
                        (defvar sortName (get 0 (get 0 dataSpec.columns)))

                        ;; This is dense, so a reminder of the types:
                        ;; groupBy :: (Eq b, Show b) => [a] -> (a -> b) -> {[a]}
                        ;; map :: {v} -> (v -> String -> a) -> [a]
                        ;; sortBy :: [{a}] -> String -> [{a}]
                        (defvar massagedData
                          (_.map (_.sortBy (_.map (_.groupBy rawData categoryName)
                                                  (fn (v k) (obj k k v (_.sortBy v sortName))))
                                           "k")
                                 (fn (d) (.v d))))
                        (_.map massagedData
                               (fn (entities)
                                 (defvar category (get categoryName (get 0 entities)))
                                 (e EntityCategory (entities entities key category)))))))

       (defvar headers (_.map (-> (if (null? dataSpec.categoryColumn)
                                    (array)
                                    (array (get 2 dataSpec.categoryColumn)))
                                  (.concat (_.map dataSpec.columns (fn (v) (get 2 v)))))
                              (fn (label idx) (e "th" (key idx) label))))
       (e "div" (className "table-responsive")
         (e "table" (className "table")
           (e "thead" ()
             (e "tr" ()
               headers
               (e "th" ())))
           rows))))

   ;; The Modal dialog that takes control of input and needs to be
   ;; dealt with before the rest of the page is functional.
   (defcomponent Modal
     propTypes
     (obj
      canClose React.PropTypes.bool.isRequired
      title React.PropTypes.node.isRequired
      buttons React.PropTypes.node
      children React.PropTypes.node.isRequired)

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

   ;; An action modal, with custom content, a Cancel button and an
   ;; action button.
   (defcomponent ActionModal
     propTypes
     (obj
      actionButtonType React.PropTypes.string
      actionButtonStyle React.PropTypes.string.isRequired
      actionButtonLabel React.PropTypes.node.isRequired
      title React.PropTypes.node.isRequired
      children React.PropTypes.node.isRequired
      next React.PropTypes.func.isRequired)

     getInitialState
     (fn () (obj spinner 0))
     render
     (fn ()
       (defvar buttons
         (e "div" ()
           ;; Note that we must render both elements in both cases
           ;; because we don't want to deal with re-attaching event
           ;; handlers.
           (e "img" (width 16 height 16 src "/static/res/loading.gif" style (obj display (if this.state.spinner "inline" "none"))))
           (e "div" (style (obj display (if this.state.spinner "none" "block")))
             (e "button" (type "button" className "btn btn-default" "data-dismiss" "modal") "Cancel")
             (e "button" (type (|| this.props.actionButtonType "button") className (+ "btn btn-" this.props.actionButtonStyle) id "actionButton") this.props.actionButtonLabel))))
       (e Modal (canClose true title this.props.title buttons buttons)
         this.props.children))

     ;; Register click handler.
     componentDidMount
     (fn ()
       (defvar that this)
       (defun setSpinner (v) (that.setState (obj spinner v)))
       (-> ($ "#actionButton")
           (.on "click" (fn (e)
                          (e.preventDefault)
                          (that.props.next (fn () (-> ($ "#modal") (.modal "hide"))) setSpinner))))))

   ;; An editor for records.
   (defcomponent RecordEditor
     propTypes
     (obj
      entityTypeHumanName React.PropTypes.string.isRequired
      entityTypeMachineName React.PropTypes.string.isRequired
      entity React.PropTypes.object
      children React.PropTypes.node.isRequired)

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
         (console.log (-> ($ "#editorForm") (.serialize)))
         (setSpinner 1)
         ($.ajax endpoint (obj type method
                               data (-> ($ "#editorForm") (.serialize))
                               success hideModal
                               error (fn (jqxhr)
                                       (setSpinner 0)
                                       (console.log "http error")
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
      entity React.PropTypes.object)

     render
     (fn ()
       (e RecordEditor (entity this.props.entity entityTypeHumanName "CCA" entityTypeMachineName "ccas")
         (e "div" (className "form-group")
           (e "label" (htmlFor "name") "CCA Name")
           (e "input" (type "text" className "form-control" name "name" placeholder "e.g. Infocomm Club" defaultValue (if this.props.entity this.props.entity.name ""))))
         (e "div" (className "form-group")
           (e "label" (htmlFor "category") "CCA Category")
           (e "input" (type "text" className "form-control" name "category" placeholder "e.g. Clubs and Societies" defaultValue (if this.props.entity this.props.entity.category "")))))))

   (defcomponent SubjectEditor
     propTypes
     (obj
      entity React.PropTypes.object)

     getInitialState
     (fn ()
       (obj compulsory (if (! this.props.entity) false (null? this.props.entity.code))))

     render
     (fn ()
       (console.log this.props.entity)
       (var that this)
       (defun compulsoryChanged (event)
         (that.setState (obj compulsory event.target.checked)))
       (e RecordEditor (entity this.props.entity entityTypeHumanName "Subject" entityTypeMachineName "subjects")
         (e "div" (className "form-group")
           (e "label" (htmlFor "name") "Subject Name")
           (e "input" (type "text" className "form-control" name "name" placeholder "e.g. Mathematics (H3)" defaultValue (if this.props.entity this.props.entity.name "")))
           (e "div" (className "checkbox")
             (e "label" ()
               (e "input" (type "checkbox" onChange compulsoryChanged checked this.state.compulsory)) "This is a compulsory subject."))
           (e "div" (className "checkbox")
             (e "label" ()
               (e "input" (type "checkbox" name "science" defaultChecked (if this.props.entity this.props.entity.is_science false))) "This is a science subject.")))
         (e "div" (className "form-group")
           (e "label" (htmlFor "code") "Subject Code")
           (if this.state.compulsory
             (e "input" (type "text" className "form-control" disabled true value "" placeholder "None"))
             (e "input" (type "text" className "form-control" name "code" placeholder "e.g. MA(H3)" defaultValue (if this.props.entity this.props.entity.code ""))))
          (e "p" (className "help-block") "Compulsory subjects do not have a subject code, because since everyone takes them, there is no reason to specify them in CSV. They will, however, still appear on PDF files if they are also science subjects."))
         (e "div" (className "form-group")
           (e "label" (htmlFor "level") "Applies To")
           (e "div" (className "checkbox")
             (_.map (array 1 2 3 4 5 6)
                    (fn (lv)
                      (defvar checked (if that.props.entity (!= -1 (_.indexOf that.props.entity.level lv)) false))
                      (e "label" (key lv className "checkbox-inline")
                        (e "input" (type "checkbox" name "level" value lv defaultChecked checked)) "Year " lv)))))
         (e "div" (className "checkbox")
           (e "label" ()
             (e "input" (type "checkbox" name "force" defaultChecked false)) "Force the operation to continue despite errors (not recommended).")))))

   (defcomponent TestDecoder
     getInitialState
     (fn () (obj decodeResult null))
     render
     (fn ()
       (defvar that this)
       (defun next (hideModal setSpinner)
         (console.log (-> ($ "#decoderForm") (.serialize)))
         (setSpinner 1)
         ($.getJSON "/api/subjects/test-decode"
                    (-> ($ "#decoderForm") (.serialize))
                    (fn (data)
                      (setSpinner 0)
                      (that.setState (obj decodeResult data.data)))))
       (defun subjectsToString (ss) (-> (_.map ss (fn (s) s.name)) (.join ", ")))
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
                         (_.map this.state.decodeResult
                                (fn (ss) (e "li" () (subjectsToString ss)))))))))
           null)
         (e "form" (id "decoderForm" role "form")
           (e "p" (className "help-block") "This decoder allows you to preview decoding of a set of subject codes. You can enter a series of subject codes and see how it will be decoded.")
           (e "div" (className "form-group")
             (e "label" (htmlFor "level") "Year")
             (e "div" (className "radio")
               (_.map (array 1 2 3 4 5 6)
                      (fn (lv)
                        (e "label" (key lv className "checkbox-inline")
                          (e "input" (type "radio" name "level" value lv defaultChecked (= 1 lv))) "Year " lv)))))
           (e "div" (className "form-group")
             (e "label" (htmlFor "str") "Subject Code Combination")
             (e "input" (type "text" className "form-control" name "str" placeholder "Enter subject codes here")))))))


   ;; The CCA Delete Confirmation.
   (defcomponent DeleteConfirmation
     propTypes
     (obj
      entityTypeHumanName React.PropTypes.string.isRequired
      entityTypeMachineName React.PropTypes.string.isRequired
      entity React.PropTypes.object.isRequired)

     render
     (fn ()
       (defvar hname this.props.entityTypeHumanName)
       (defvar mname this.props.entityTypeMachineName)
       (defvar endpoint (+ (+ (+ "/api/" mname) "/") this.props.entity.id))
       (defun next (hideModal)
         ($.ajax endpoint (obj type "DELETE" complete hideModal)))
       (defvar message (+ (+ (+ (+ "Are you sure you want to delete the " hname) " “") this.props.entity.name)
                          "” from the database?"))
       (e ActionModal
           (title (+ "Delete " hname) actionButtonLabel "Delete" actionButtonStyle "danger" next next)
         (e "p" () message))))

   ;; The CCA Delete Confirmation.
   (defcomponent CcaDeleteConfirmation
     render
     (fn ()
       (e DeleteConfirmation (entity this.props.entity entityTypeHumanName "CCA" entityTypeMachineName "ccas"))))

   ;; The Subject Delete Confirmation.
   (defcomponent SubjectDeleteConfirmation
     render
     (fn ()
       (e DeleteConfirmation (entity this.props.entity entityTypeHumanName "Subject" entityTypeMachineName "subjects"))))

   ;; The Admin console homepage.
   (defcomponent AdminHomeR
     render
     (fn ()
       (e "div" (className "row")
         (e "div" (className "col-sm-11 col-md-8 col-lg-7")
           (e "h2" () "Welcome")
           (e "p" () "Welcome to the admin console for RVHS Science Lab Undertaking Project. XXX Be verbose.")
           (e "h2" () "Quick Guide")
           (e "p" () "TODO")
           (e "h2" () "API Documentation")
           (e "p" () "If you know some basics of programming, you can use it to add or remove things automatically via the HTTP JSON API.")))))

   ;; The admin console CCA page.
   (defcomponent AdminCcasR
     render
     (fn ()
       (e "div" ()
         (e "div" (className "pull-right btn-group" role "toolbar" "aria-label" "Action Buttons")
           (e "button" (id "addButton" type "button" className "btn btn-default") "Add New")
           (e "button" (id "removeAllButton" type "button" className "btn btn-default") "Remove All"))
         (e "h2" () "All CCAs")
         (e EntityTable (conn (APIConnection "/api/ccas") entityEditor CcaEditor deleteConfirmation CcaDeleteConfirmation))))

     componentDidMount
     (fn ()
       (-> ($ "#addButton")
           (.on "click"
                (fn ()
                  (React.render
                   (e CcaEditor ())
                   (getModalWrapper)))))
       (-> ($ "#removeAllButton")
           (.on "click"
                (fn ()
                  (React.render
                   (e ActionModal
                       (title "Deleting All CCAs" actionButtonLabel "Yes, Delete All" actionButtonStyle "danger" next (fn (hideModal) ($.ajax "/api/ccas" (obj type "DELETE" complete hideModal))))
                     (e "p" () "Are you sure you want to delete all CCAs currently stored in the database? This will also delete all students’ CCA information."))
                   (getModalWrapper)))))))

   (defcomponent AdminSubjectsR
     render
     (fn ()
       (e "div" ()
         (e "div" (className "pull-right btn-group" role "toolbar" "aria-label" "Action Buttons")
           (e "button" (id "testDecodeButton" type "button" className "btn btn-default") "Test Decode")
           (e "button" (id "addButton" type "button" className "btn btn-default") "Add New")
           (e "button" (id "removeAllButton" type "button" className "btn btn-default") "Remove All"))
         (e "h2" () "All Subjects")
         (e EntityTable (conn (APIConnection "/api/subjects") entityEditor SubjectEditor deleteConfirmation SubjectDeleteConfirmation))))

     componentDidMount
     (fn ()
       (-> ($ "#testDecodeButton")
           (.on "click"
                (fn ()
                  (React.render
                   (e TestDecoder ())
                   (getModalWrapper)))))
       (-> ($ "#addButton")
           (.on "click"
                (fn ()
                  (React.render
                   (e SubjectEditor ())
                   (getModalWrapper)))))
       (-> ($ "#removeAllButton")
           (.on "click"
                (fn ()
                  (React.render
                   (e ActionModal
                       (title "Deleting All Subjects" actionButtonLabel "Yes, Delete All" actionButtonStyle "danger" next (fn (hideModal) ($.ajax "/api/subjects" (obj type "DELETE" complete hideModal))))
                     (e "p" () "Are you sure you want to delete all subjects currently stored in the database? This will also delete all students’ subject information."))
                   (getModalWrapper))))))
       )

   (defcomponent AdminTeachersR)

   (defcomponent AdminStudentsR)

   (defvar pageSpec (obj "/admin" (obj pageName "Home"
                                       component AdminHomeR
                                       dataSpec null)
                         "/admin/ccas" (obj pageName "Manage CCAs"
                                            component AdminCcasR
                                            dataSpec (obj categoryColumn (array "category" _.identity "CCA Category")
                                                          columns (array (array "name" _.identity "CCA Name"))))
                         "/admin/subjects" (obj pageName "Manage Subjects"
                                                component AdminSubjectsR
                                                dataSpec (obj categoryColumn (array "level" (fn (ls) (-> (_.map ls (fn (l) (+ "Year " l))) (.join ", "))) "Applies To")
                                                              columns (array
                                                                       (array "name" _.identity "Subject Name")
                                                                       (array "code" (fn (v) (if v (e "code" () v) (e "i" () "(None; Compulsory Subject)"))) "Subject Code")

                                                                       (array "is_science" (fn (b) (if b "Yes" "No")) "Science Subject?"))))
                         "/admin/teachers" (obj pageName "Manage Teachers"
                                                component AdminTeachersR
                                                dataSpec null)
                         "/admin/students" (obj pageName "Manage Students"
                                                component AdminStudentsR
                                                dataSpec null)))

   (defcomponent Page
     render
     (fn ()
       (defvar pathname window.location.pathname)
       (defvar tabs
         (_.map pageSpec
                (fn (page route)
                  (e "li" (key route role "presentation" className (if (= route pathname) "active" ""))
                    (e "a" (href (if (= route pathname) "#" route))
                      (.pageName page))))))
       (e "div" (id "content-wrapper")
         (e "div" (id "modal-wrapper"))
         (e "div" (className "container")
           (e "div" (className "page-header")
             (e "h1" () "RVHS Science Lab Undertaking — For Teachers and Administrators"))
           (e "p" () "You are logged in as xxx.")
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
;;; eval: (add-hook 'after-save-hook (lambda () (shell-command "lispy admin.ls") (shell-command "/Library/Internet\\ Plug-Ins/JavaAppletPlugin.plugin/Contents/Home/bin/java -jar ../../scratch/closure-compiler/compiler.jar --compilation_level SIMPLE_OPTIMIZATIONS --language_in ECMASCRIPT5_STRICT --js admin.js > admin.min.js")) nil t)
;;; End:
